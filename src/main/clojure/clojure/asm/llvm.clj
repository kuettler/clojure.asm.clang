(ns clojure.asm.llvm
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.asm.api :refer [generate-wrapper-api]])
  (:import (clojure.asm LLVMLibrary)
           (com.ochafik.lang.jnaerator.runtime NativeSize NativeSizeByReference)
           (com.sun.jna Callback Pointer PointerType Structure)
           (com.sun.jna.ptr IntByReference LongByReference PointerByReference)
           (java.nio ByteBuffer IntBuffer)))

(generate-wrapper-api clojure.asm.LLVMLibrary
                      :object-oriented? true
                      :static-object clojure.asm.LLVMLibrary/INSTANCE)

(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMAttribute)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMOpcode)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMTypeKind)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMLinkage)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMVisibility)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMDLLStorageClass)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMCallConv)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMIntPredicate)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMRealPredicate)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMLandingPadClauseTy)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMThreadLocalMode)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMAtomicOrdering)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMAtomicRMWBinOp)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMDiagnosticSeverity)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMByteOrdering)
(generate-wrapper-api clojure.asm.LLVMLibrary$LLVMVerifierFailureAction)

(defn LLVMInitializeNativeTarget
  []
  (LLVMLinkInMCJIT)
  (LLVMLinkInInterpreter)
  (LLVMInitializeX86TargetInfo)
  (LLVMInitializeX86Target)
  (LLVMInitializeX86TargetMC)
  (LLVMInitializeX86AsmPrinter)
  (LLVMInitializeX86AsmParser)
  (LLVMInitializeX86Disassembler)
  (LLVMEnablePrettyStackTrace))

(LLVMInitializeNativeTarget)

(def ^:dynamic *module* (LLVMModuleCreateWithName "user"))
(def ^:dynamic *builder* (LLVMCreateBuilder))
(def ^:dynamic *context* (LLVMGetGlobalContext))

(def ^:dynamic *engine*
  (-> (doto (PointerByReference.)
        (LLVMCreateExecutionEngineForModule *module* (into-array String [])))
      (.getValue)
      (clojure.asm.LLVMLibrary$LLVMExecutionEngineRef.)))

(def ^:dynamic *pass-manager* nil)

(def ^:dynamic *pass-registry*
  (doto (LLVMGetGlobalPassRegistry)
    (LLVMInitializeCore)
    (LLVMInitializeTransformUtils)
    (LLVMInitializeScalarOpts)
    (LLVMInitializeVectorization)
    (LLVMInitializeInstCombine)
    (LLVMInitializeInstrumentation)
    (LLVMInitializeIPO)
    (LLVMInitializeAnalysis)
    (LLVMInitializeIPA)
    (LLVMInitializeCodeGen)
    (LLVMInitializeTarget)))

(defmethod print-method clojure.asm.LLVMLibrary$LLVMModuleRef
  [x writer]
  (.write writer (LLVMPrintModuleToString x)))

(defmethod print-method clojure.asm.LLVMLibrary$LLVMValueRef
  [x writer]
  (.write writer (LLVMPrintValueToString x)))

(defmethod print-method clojure.asm.LLVMLibrary$LLVMTypeRef
  [x writer]
  (.write writer (LLVMPrintTypeToString x)))

(ns-unmap *ns* 'JNA_LIBRARY_NAME)
(ns-unmap *ns* 'JNA_LIBRARY_PATH)
(ns-unmap *ns* 'INSTANCE)
