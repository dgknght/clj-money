(ns clj-money.web.cli-auth
  (:require [clojure.tools.logging :as log]
            [ring.util.response :as res]
            [hiccup.page :refer [html5 include-js]]
            [hiccup.form :as form]
            [clj-money.config :refer [env]]))

(defn- head []
  [:head
   [:meta  {:charset "utf-8"}]
   [:meta  {:http-equiv "X-UA-Compatible" :content "IE=edge"}]
   [:meta  {:name "viewport"
            :content "width=device-width, initial-scale=1"}]
   [:meta  {:name "description"
            :content "CLI Authorization"}]
   [:link  {:rel "icon" :href "images/logo.svg"}]
   [:title "CLI Authorization - " (env :application-name "clj-money")]
   (include-js "https://unpkg.com/@popperjs/core@2")
   (include-js "js/bootstrap.min.js")
   [:link {:rel "stylesheet" :href "/css/site.css"}]])

(defn- cli-auth-page
  [user-code error]
  [:html.h-100 {:lang "en"
                :data-bs-theme "dark"}
   (head)
   [:body.h-100
    [:div.container.mt-5
     [:div.row.justify-content-center
      [:div.col-md-6
       [:div.card
        [:div.card-header
         [:h3 "Authorize CLI Application"]]
        [:div.card-body
         (when error
           [:div.alert.alert-danger {:role "alert"} error])
         [:p "Enter the code displayed in your CLI application to
              authorize it to access your account."]
         [:form#auth-form {:method "post"}
          [:div.mb-3
           [:label.form-label {:for "user_code"} "Authorization Code"]
           [:input.form-control
            {:type "text"
             :id "user_code"
             :name "user_code"
             :value (or user-code "")
             :placeholder "XXXXXXXX"
             :pattern "[A-Z2-9]{8}"
             :maxlength "8"
             :required true
             :autofocus true
             :style "text-transform: uppercase; font-size: 1.5rem;
                     letter-spacing: 0.3rem; text-align: center;"}]]
          [:div.d-grid.gap-2
           [:button.btn.btn-primary
            {:type "button"
             :id "approve-btn"}
            "Authorize"]
           [:button.btn.btn-outline-secondary
            {:type "button"
             :id "deny-btn"}
            "Deny"]]]]
        [:div.card-footer.text-muted
         [:small "You are authorizing access for a CLI application.
                  Only continue if you initiated this request."]]]]]]
    [:script
     "
     const approveBtn = document.getElementById('approve-btn');
     const denyBtn = document.getElementById('deny-btn');
     const userCodeInput = document.getElementById('user_code');

     async function handleAuth(approve) {
       const userCode = userCodeInput.value.toUpperCase();
       if (!userCode || userCode.length !== 8) {
         alert('Please enter a valid 8-character code');
         return;
       }

       const endpoint = approve ? '/api/cli/auth/approve'
                                : '/api/cli/auth/deny';

       try {
         const response = await fetch(endpoint, {
           method: 'POST',
           headers: {
             'Content-Type': 'application/json',
           },
           body: JSON.stringify({ user_code: userCode }),
           credentials: 'include'
         });

         const data = await response.json();

         if (response.ok) {
           document.body.innerHTML = '<div class=\"container mt-5\">' +
             '<div class=\"row justify-content-center\">' +
             '<div class=\"col-md-6\">' +
             '<div class=\"alert alert-success\" role=\"alert\">' +
             '<h4 class=\"alert-heading\">Success!</h4>' +
             '<p>' + data.message + '</p>' +
             '<hr><p class=\"mb-0\">You may close this window now.</p>' +
             '</div></div></div></div>';
         } else {
           alert(data.error || 'An error occurred');
         }
       } catch (error) {
         alert('Network error: ' + error.message);
       }
     }

     approveBtn.addEventListener('click', () => handleAuth(true));
     denyBtn.addEventListener('click', () => handleAuth(false));

     userCodeInput.addEventListener('keypress', function(e) {
       if (e.key === 'Enter') {
         e.preventDefault();
         handleAuth(true);
       }
     });

     userCodeInput.addEventListener('input', function(e) {
       e.target.value = e.target.value.toUpperCase();
     });
     "]]])

(defn show-auth-page
  "Displays the CLI authorization page"
  [{:keys [query-params]}]
  (log/info "CLI auth page requested" query-params)
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8"}
   :body (html5 (cli-auth-page (:user_code query-params) nil))})

(def routes
  ["/authorize" {:get {:handler show-auth-page}}])
