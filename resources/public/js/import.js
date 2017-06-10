(function () {
  angular.module('clj-money-import', [])
    .factory('apiClient', ['$http', function($http) {
      return {
        createImport: function(attributes) {
          var data = new FormData();
          for(var key in attributes)
            data.append(key, attributes[key]);
          return $http.post("/api/imports", data, {
            headers: {"Content-Type": undefined}
          });
        }
      };
    }])
    .directive('fileModel', ['$parse', function($parse) {
      return {
        restrict: 'A',
        link: function(scope, element, attrs) {
          var model = $parse(attrs.fileModel);
          var modelSetter = model.assign;

          element.bind('change', function() {
            scope.$apply(function() {
              modelSetter(scope, element[0].files[0]);
            });
          });
        }
      };
    }])
    .controller('ImportController', ['$scope', 'apiClient', function($scope, apiClient) {
      $scope.startImport = function() {
          apiClient.createImport({
            name: $scope.entityName,
            "source-file": $scope.sourceFile
          }).then(function(response) {
          console.log("response");
          console.log(response);
        });
      };
    }])
  .run(['$http', function($http) {
    $http.defaults.headers.common['Content-Type'] = "application/json";
    $http.defaults.headers.common['Accept'] = 'application/json';
  }]);
})();
