(function () {
  angular.module('clj-money-import', [])
    .controller('ImportController', ['$scope', '$http', function($scope, $http) {
      $scope.startImport = function() {
        $http.get("/api/imports/1").then(function(response) {
          console.log("response");
          console.log(response);
        });
      };
    }])
  .run(['$http', function($http) {
    $http.defaults.headers.common['Content-Type'] = "application/json";
    $http.defaults.headers.common['X-Default-Test'] = 'defaults-test';
  }]);
})();
