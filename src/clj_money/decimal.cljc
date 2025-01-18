(ns clj-money.decimal
  #?(:cljs (:require[dgknght.app-lib.decimal :as decimal])))

^{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(def d
  #?(:clj bigdec
     :cljs decimal/->decimal))
