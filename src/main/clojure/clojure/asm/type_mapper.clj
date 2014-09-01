(ns clojure.asm.type-mapper
  (:require [clojure.asm.llvm :as llvm]
            [clojure.asm.clang :as clang])
  (:import (clojure.asm ClangLibrary$CXType$ByValue)))

(declare clang->llvm)

(defonce cache (ref {}))

(defn update-cache
  [clang-type llvm-type]
  (dosync
    (if (integer? clang-type)
      (alter cache assoc clang-type llvm-type)
      (alter cache assoc (-> (clang/clang-get-type-spelling clang-type)
                             (clang/clang-get-cstring)
                             (hash)) llvm-type)))
  llvm-type)

(defmulti -clang->llvm (fn [^ClangLibrary$CXType$ByValue clang-type]
                         (.-kind clang-type)))

(defmethod -clang->llvm clang/CXType_Void
  [type]
  (update-cache clang/CXType_Void (llvm/LLVMVoidType)))

(defmethod -clang->llvm clang/CXType_Bool
  [type]
  (update-cache clang/CXType_Bool (llvm/LLVMInt1Type)))

(defmethod -clang->llvm clang/CXType_Char_S
  [type]
  (update-cache clang/CXType_Char_S (llvm/LLVMInt8Type)))

(defmethod -clang->llvm clang/CXType_Char_U
  [type]
  (update-cache clang/CXType_Char_U (llvm/LLVMInt8Type)))

(defmethod -clang->llvm clang/CXType_Char16
  [type]
  (update-cache clang/CXType_Char16 (llvm/LLVMInt16Type)))

(defmethod -clang->llvm clang/CXType_Char32
  [type]
  (update-cache clang/CXType_Char32 (llvm/LLVMInt32Type)))

(defmethod -clang->llvm clang/CXType_UShort
  [type]
  (update-cache clang/CXType_UShort (llvm/LLVMInt16Type)))

(defmethod -clang->llvm clang/CXType_UInt
  [type]
  (update-cache clang/CXType_UInt (llvm/LLVMInt32Type)))

(defmethod -clang->llvm clang/CXType_ULong
  [type]
  (update-cache clang/CXType_ULong (llvm/LLVMInt64Type)))

(defmethod -clang->llvm clang/CXType_ULongLong
  [type]
  (update-cache clang/CXType_ULongLong (llvm/LLVMInt64Type)))

(defmethod -clang->llvm clang/CXType_Short
  [type]
  (update-cache clang/CXType_Short (llvm/LLVMInt16Type)))

(defmethod -clang->llvm clang/CXType_Int
  [type]
  (update-cache clang/CXType_Int (llvm/LLVMInt32Type)))

(defmethod -clang->llvm clang/CXType_Long
  [type]
  (update-cache clang/CXType_Long (llvm/LLVMInt64Type)))

(defmethod -clang->llvm clang/CXType_LongLong
  [type]
  (update-cache clang/CXType_LongLong (llvm/LLVMInt64Type)))

(defmethod -clang->llvm clang/CXType_Int128
  [type]
  (update-cache clang/CXType_Int128 (llvm/LLVMIntType 128)))

(defmethod -clang->llvm clang/CXType_Float
  [type]
  (update-cache clang/CXType_Float (llvm/LLVMFloatType)))

(defmethod -clang->llvm clang/CXType_Double
  [type]
  (update-cache clang/CXType_Double (llvm/LLVMDoubleType)))

(defmethod -clang->llvm clang/CXType_LongDouble
  [type]
  (update-cache clang/CXType_LongDouble
                (or (llvm/LLVMX86FP80Type) (llvm/LLVMFP128Type))))

(defmethod -clang->llvm clang/CXType_Pointer
  [type]
  (let [pointee (clang/clang-get-pointee-type type)
        pointee-type (if (= (llvm/LLVMVoidType) (clang->llvm pointee))
                       (llvm/LLVMInt8Type)
                       (clang->llvm pointee))]
    (update-cache type (llvm/LLVMPointerType pointee-type 0))))

(defmethod -clang->llvm clang/CXType_Record
  [t]
  (update-cache t (let [visitor (resolve 'clojure.asm.reflector/visit-fields)
                        fields (visitor t)]
                    (llvm/LLVMStructType
                     (into-array clojure.asm.LLVMLibrary$LLVMTypeRef
                                 (or (seq (map :type fields))
                                     [(llvm/LLVMVoidType)]))
                     (count fields)
                     false))))

(defmethod -clang->llvm clang/CXType_FunctionProto
  [t]
  (let [f (llvm/LLVMFunctionType
           (clang->llvm (clang/clang-get-result-type t))
           (into-array clojure.asm.LLVMLibrary$LLVMTypeRef
                       (or (seq (map
                                 (fn [n]
                                   (clang->llvm (clang/clang-get-arg-type t n)))
                                 (range (clang/clang-get-num-arg-types t))))
                           [(llvm/LLVMVoidType)]))
           (clang/clang-get-num-arg-types t)
           (if (== (clang/clang-is-function-type-variadic t) 1) true false))]
    (update-cache t f)))

(defmethod -clang->llvm clang/CXType_Invalid
  [type]
  (throw (ex-info "Invalid type encountered." {:type type})))

(defmethod -clang->llvm :default
  [type]
  (llvm/LLVMPointerType (llvm/LLVMInt8Type) 0))

(defn cache-lookup
  [^ClangLibrary$CXType$ByValue type]
  (or (get @cache (.-kind type))
      (get @cache (-> (clang/clang-get-type-spelling type)
                      (clang/clang-get-cstring)
                      (hash)))))

(defn clang->llvm
  [^ClangLibrary$CXType$ByValue type]
  (if-let [llvm-type (cache-lookup type)]
    llvm-type
    (-clang->llvm type)))
