(ns clj-money.icons-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is]])
            #?(:clj [clojure.pprint :refer [pprint]]
               :cljs [cljs.pprint :refer [pprint]])
            [clj-money.icons :as icons]))

(deftest generate-an-icon
  (is (= [:svg.bi {:width 24
                   :height 24
                   :fill "currentColor"}
          [:use {:href "/images/bootstrap-icons.svg#pencil"}]]
         (icons/icon :pencil))
      "The default size is medium")
  (is (= [:svg.bi {:width 24
                   :height 24
                   :fill "currentColor"}
          [:use {:href "/images/bootstrap-icons.svg#pencil"}]]
         (icons/icon :pencil :size :medium))
      "A medium icon is 24x24")
  (is (= [:svg.bi {:width 40
                   :height 40
                   :fill "currentColor"}
          [:use {:href "/images/bootstrap-icons.svg#pencil"}]]
         (icons/icon :pencil :size :large))
      "A large icon is 40x40")
  (is (= [:svg.bi {:width 16
                   :height 16
                   :fill "currentColor"}
          [:use {:href "/images/bootstrap-icons.svg#pencil"}]]
         (icons/icon :pencil :size :small))
      "A small icon is 16x16")
  (is (= [:svg.bi {:width 32
                   :height 32
                   :fill "currentColor"}
          [:use {:href "/images/bootstrap-icons.svg#pencil"}]]
         (icons/icon :pencil :size :medium-large))
      "A medium-large icon is 32x32"))

(deftest generate-an-icon-with-text
  (is (= [:span.d-flex.align-items-center
          [:svg.bi {:width 40
                    :height 40
                    :fill "currentColor"}
           [:use {:href "/images/bootstrap-icons.svg#pencil"}]]
          [:span.ms-2 "Edit"]]
         (icons/icon-with-text :pencil "Edit" :size :large))))
