(ns clojure.asm.jit
  (:require [clojure.asm.llvm :refer :all])
  (:import (clojure.asm LLVMLibrary
                        LLVMLibrary$LLVMTypeRef
                        LLVMLibrary$LLVMValueRef
                        LLVMLibrary$LLVMBasicBlockRef
                        LLVMLibrary$LLVMCallConv
                        LLVMLibrary$LLVMGenericValueRef
                        LLVMLibrary$LLVMExecutionEngineRef
                        LLVMLibrary$LLVMMCJITCompilerOptions
                        LLVMLibrary$LLVMMCJITMemoryManagerRef
                        LLVMLibrary$LLVMMemoryManagerAllocateCodeSectionCallback
                        LLVMLibrary$LLVMMemoryManagerAllocateDataSectionCallback
                        LLVMLibrary$LLVMMemoryManagerFinalizeMemoryCallback
                        LLVMLibrary$LLVMMemoryManagerDestroyCallback
                        LLVMLibrary$LLVMCodeModel)
           (com.sun.jna Function Memory Native Pointer)
           (com.sun.jna.ptr PointerByReference)
           (com.ochafik.lang.jnaerator.runtime NativeSize)
           (org.bridj TypedPointer)
           (org.bridj.ann Convention Convention$Style)
           (java.lang.reflect Type)
           (sun.misc Unsafe)))

(def ^:dynamic *section-head*)

(def get-page-size
  (memoize (fn []
             (-> (doto (.getDeclaredField Unsafe "theUnsafe")
                   (.setAccessible true))
                 ^Unsafe (.get nil)
                 (.pageSize)))))

(defn allocate-code-section
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerAllocateCodeSectionCallback
    (apply [_ opaque size alignment section-id section-name]
      (f opaque size alignment section-id section-name))))

(defn allocate-data-section
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerAllocateDataSectionCallback
    (apply [_ opaque size alignment section-id section-name read-only?]
      (f opaque size alignment section-id section-name read-only?))))

(defn finalize-memory
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerFinalizeMemoryCallback
    (apply [_ opaque err-msg]
      (f opaque err-msg))))

(defn destroy
  [f]
  (reify
    LLVMLibrary$LLVMMemoryManagerDestroyCallback
    (apply [_ opaque]
      (f opaque))))

(defn lisp-code-section-allocator
  [opaque size alignment section-id section-name]
  (let [page-size (get-page-size)
        start (Memory. (bit-and-not (+ size (dec page-size)) page-size))
        section {:start start
                 :size size
                 :next *section-head*}]
    (set! *section-head* section)
    start))

(defn lisp-data-section-allocator
  [opaque size alignment section-id section-name read-only?]
  (lisp-code-section-allocator opaque size alignment section-id section-name))

(defn lisp-memory-finalizer
  [opaque err-msg]
  true)

(defn lisp-memory-destroyer
  [opaque])

(defn lisp-memory-manager
  []
  (let [code-allocator (allocate-code-section lisp-code-section-allocator)
        data-allocator (allocate-data-section lisp-data-section-allocator)
        memory-finalizer (finalize-memory lisp-memory-finalizer)
        memory-destroyer (destroy lisp-memory-destroyer)]
    (LLVMCreateSimpleMCJITMemoryManager Pointer/NULL
                                        code-allocator
                                        data-allocator
                                        memory-finalizer
                                        memory-destroyer)))

(defn ^LLVMLibrary$LLVMMCJITCompilerOptions mcjit-compiler-options
  ([] (mcjit-compiler-options 0))
  ([opt-level]
     (mcjit-compiler-options opt-level 1))
  ([opt-level code-model]
     (mcjit-compiler-options opt-level code-model false))
  ([opt-level code-model no-fp-elim]
     (mcjit-compiler-options opt-level code-model no-fp-elim true))
  ([opt-level code-model no-fp-elim fast-isel]
     (mcjit-compiler-options opt-level code-model no-fp-elim fast-isel
                             (lisp-memory-manager)))
  ([opt-level code-model no-fp-elim fast-isel memory-manager]
     (let [options (LLVMLibrary$LLVMMCJITCompilerOptions. opt-level
                                                          code-model
                                                          no-fp-elim
                                                          fast-isel
                                                          memory-manager)]
       (doto options
         (LLVMInitializeMCJITCompilerOptions (NativeSize. (.size options)))))))

(defn mcjit
  [module]
  (let [ee (PointerByReference.)
        options (mcjit-compiler-options)
        sizeof (NativeSize. (.size options))
        error (PointerByReference.)]
    (when-not (LLVMCreateMCJITCompilerForModule ee module options sizeof error)
      (LLVMLibrary$LLVMExecutionEngineRef. (.getValue ee)))))

(defn values
  [module]
  (take-while (complement nil?)
              (iterate LLVMGetNextFunction (LLVMGetFirstFunction module))))

(defn declarations
  [module]
  (filter LLVMIsDeclaration (values module)))

(defn functions
  [module]
  (remove LLVMIsDeclaration (values module)))

(defn compiled-functions
  [engine module]
  (map (partial LLVMGetPointerToGlobal engine) (functions module)))

(defmacro with-mcjit
  [& body]
  `(binding [*engine* (mcjit *module*)]
     (let [ret# (do ~@body)]
       ;; (LLVMDisposeExecutionEngine *engine*)
       ret#)))

(defmacro with-optimization-passes
  [passes & body]
  `(binding [*pass-manager* (LLVMCreateFunctionPassManagerForModule *module*)]
     (LLVMAddTargetData (LLVMGetExecutionEngineTargetData *engine*)
                        *pass-manager*)
     (doto *pass-manager*
       ~@(map list passes))
     (LLVMInitializeFunctionPassManager *pass-manager*)
     (let [ret# (do ~@body)]
       (LLVMDisposePassManager *pass-manager*)
       ret#)))

(defn compile-module
  []
  (with-mcjit
    (with-optimization-passes
      [LLVMAddBasicAliasAnalysisPass
       LLVMAddPromoteMemoryToRegisterPass
       LLVMAddReassociatePass
       LLVMAddGVNPass
       LLVMAddCFGSimplificationPass
       LLVMAddTailCallEliminationPass]
      (let [xs (transient [])]
        (doseq [x (functions *module*)]
          (LLVMRunFunctionPassManager *pass-manager* x)
          (conj! xs (LLVMGetPointerToGlobal *engine* x)))
        (persistent! xs)))))

(defn type-array
  [types]
  (into-array LLVMLibrary$LLVMTypeRef types))

(defn value-array
  [values]
  (into-array LLVMLibrary$LLVMValueRef values))

(defn basic-block-array
  [basic-blocks]
  (into-array LLVMLibrary$LLVMBasicBlockRef basic-blocks))

(defn legal-primitive-type?
  [type]
  (or (= (LLVMInt64Type) type)
      (= (LLVMDoubleType) type)
      (= (LLVMPointerType (LLVMInt8Type) 0) type)))

(defn function-type
  ([] (function-type (LLVMPointerType (LLVMInt8Type) 0)))
  ([return-type] (function-type return-type []))
  ([return-type parameter-types]
     ;; (assert (every? legal-primitive-type?
     ;;                 (cons return-type parameter-types))
     ;;         "Only long and double primitives are supported")
     (LLVMFunctionType return-type
                       (if (empty? parameter-types)
                         (type-array [(LLVMVoidType)])
                         (type-array parameter-types))
                       (count parameter-types)
                       false)))

(defn maybe-tag
  [x]
  (or (and (instance? clojure.lang.IObj x) (:tag (meta x)))
      (LLVMPointerType (LLVMInt8Type) 0)))

(defn emit-fac
  []
  (binding [*module* (LLVMModuleCreateWithName "fac")
            *builder* (LLVMCreateBuilder)]
    (let [fn-type (function-type (LLVMInt64Type) [(LLVMInt64Type)])
          fac (doto (LLVMAddFunction *module* "fac" fn-type)
                (LLVMSetFunctionCallConv LLVMFastCallConv))
          n (LLVMGetParam fac 0)
          entry (LLVMAppendBasicBlock fac "entry")
          iftrue (LLVMAppendBasicBlock fac "iftrue")
          iffalse (LLVMAppendBasicBlock fac "iffalse")
          end (LLVMAppendBasicBlock fac "end")
          res-iftrue (LLVMConstInt (LLVMInt64Type) 1 false)]
      (LLVMPositionBuilderAtEnd *builder* entry)
      (let [if-cmp (LLVMBuildICmp *builder* LLVMIntEQ n
                                  (LLVMConstInt (LLVMInt64Type) 0 false) "")]
        (LLVMBuildCondBr *builder* if-cmp  iftrue iffalse)
        (LLVMPositionBuilderAtEnd *builder* iftrue)
        (LLVMBuildBr *builder* end)
        (LLVMPositionBuilderAtEnd *builder* iffalse)
        (let [n-minus (LLVMBuildSub *builder* n
                                    (LLVMConstInt (LLVMInt64Type) 1 false)
                                    "")
              call-fac (LLVMBuildCall *builder* fac (value-array [n-minus])
                                      1 "")
              res-iffalse (LLVMBuildMul *builder* n call-fac "")
              err (into-array String [])]
          (LLVMBuildBr *builder* end)
          (LLVMPositionBuilderAtEnd *builder* end)
          (let [res (LLVMBuildPhi *builder* (LLVMInt64Type) "result")
                phi-vals (value-array [res-iftrue res-iffalse])
                phi-blocks (basic-block-array [iftrue iffalse])]
            (LLVMAddIncoming res phi-vals phi-blocks 2)
            (LLVMBuildRet *builder* res)
            (if-not (LLVMVerifyModule *module* LLVMPrintMessageAction err)
              (Function/getFunction (first (compile-module)))
              fac)))))))
