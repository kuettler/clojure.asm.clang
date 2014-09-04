(ns clojure.asm.clang
  (:require [clojure.asm.api :refer [generate-wrapper-api]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set])
  (:import (clojure.asm ClangLibrary
                        ClangLibrary$CXTranslationUnit
                        ClangLibrary$CXCursorVisitor
                        ClangLibrary$CXCursor$ByValue
                        ClangLibrary$CXCursorKind
                        ClangLibrary$CXString$ByValue
                        ClangLibrary$CXType$ByValue
                        ClangLibrary$CXTypeKind)
           (com.sun.jna Pointer)
           (com.sun.jna.ptr PointerByReference)
           (java.nio IntBuffer)))

(generate-wrapper-api clojure.asm.ClangLibrary
                      :object-oriented? true
                      :static-object clojure.asm.ClangLibrary/INSTANCE
                      :lispify-names? true)

(generate-wrapper-api clojure.asm.ClangLibrary$CXAvailabilityKind)
(generate-wrapper-api clojure.asm.ClangLibrary$CXGlobalOptFlags)
(generate-wrapper-api clojure.asm.ClangLibrary$CXCursorKind)
(generate-wrapper-api clojure.asm.ClangLibrary$CXCompilationDatabase_Error)
(generate-wrapper-api clojure.asm.ClangLibrary$CXDiagnosticSeverity)
(generate-wrapper-api clojure.asm.ClangLibrary$CXLoadDiag_Error)
(generate-wrapper-api clojure.asm.ClangLibrary$CXDiagnosticDisplayOptions)
(generate-wrapper-api clojure.asm.ClangLibrary$CXTranslationUnit_Flags)
(generate-wrapper-api clojure.asm.ClangLibrary$CXSaveTranslationUnit_Flags)
(generate-wrapper-api clojure.asm.ClangLibrary$CXSaveError)
(generate-wrapper-api clojure.asm.ClangLibrary$CXReparse_Flags)
(generate-wrapper-api clojure.asm.ClangLibrary$CXTUResourceUsageKind)
(generate-wrapper-api clojure.asm.ClangLibrary$CXTypeKind)
(generate-wrapper-api clojure.asm.ClangLibrary$CXCallingConv)
(generate-wrapper-api clojure.asm.ClangLibrary$CXTypeLayoutError)
(generate-wrapper-api clojure.asm.ClangLibrary$CXRefQualifierKind)
(generate-wrapper-api clojure.asm.ClangLibrary$CX_CXXAccessSpecifier)
(generate-wrapper-api clojure.asm.ClangLibrary$CXChildVisitResult)
(generate-wrapper-api clojure.asm.ClangLibrary$CXNameRefFlags)
(generate-wrapper-api clojure.asm.ClangLibrary$CXTokenKind)
(generate-wrapper-api clojure.asm.ClangLibrary$CXCompletionChunkKind)
(generate-wrapper-api clojure.asm.ClangLibrary$CXCodeComplete_Flags)
(generate-wrapper-api clojure.asm.ClangLibrary$CXCompletionContext)
(generate-wrapper-api clojure.asm.ClangLibrary$CXVisitorResult)
(generate-wrapper-api clojure.asm.ClangLibrary$CXResult)

(def ^:dynamic *include-paths* ["/usr/include" "/usr/local/include"
                                "/usr/local/lib/clang/3.6.0/include"])
(def ^:dynamic *args* (into-array (map #(str "-I" %) *include-paths*)))
(def ^:dynamic *index* nil)
(def ^:dynamic *translation-unit* nil)
(def ^:dynamic *path-separator* (System/getProperty "path.separator"))
(def ^:dynamic *line-separator* (System/getProperty "line.separator"))
(def ^:dynamic *file-separator* (System/getProperty "file.separator"))

(defmethod print-method clojure.asm.ClangLibrary$CXString$ByValue
  [x ^java.io.Writer writer]
  (.write writer ^String (clang-get-cstring x)))

(defmethod print-method clojure.asm.ClangLibrary$CXType$ByValue
  [x ^java.io.Writer writer]
  (.write writer (pr-str (clang-get-type-spelling x))))

(defmethod print-method clojure.asm.ClangLibrary$CXTypeKind
  [x ^java.io.Writer writer]
  (.write writer (pr-str (clang-get-type-kind-spelling x))))

(clang-enable-stack-traces)
(clang-toggle-crash-recovery (int 1))
