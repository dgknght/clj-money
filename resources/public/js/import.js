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
        },
        getImport: function(importId) {
          return $http.get("/api/imports/" + importId);
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
      $scope.activeImport = null;
      $scope.alerts = [];
      $scope.statusMessage = null;

      var importIsComplete = function(imp) {
        progress = _.pick(imp.progress, 'account', 'transaction', 'budget');
        if (_.isEmpty(progress))
          return false;
        return _.every(progress, function(prop) {
          return prop.total == prop.imported;
        });
      };

      var trackImportProgress = function() {
        var trackingId = window.setInterval(function() {
          apiClient.getImport($scope.activeImport.id).then(function(response) {
            $scope.activeImport = response.data;
            if (importIsComplete($scope.activeImport)) {
              $scope.statusMessage = null;
              $scope.alerts.push({
                message: "Import complete.",
                level: 'success'
              });
              window.clearInterval(trackingId);
            }
          }, function(error) {
            console.log("Unable to get the updated import");
            console.log(error);
            $scope.statusMessage = null;
            $scope.alerts.push({
              message: "Import failed: " + error.statusText,
              level: 'danger'
            });
            window.clearInterval(trackingId);
          });
        }, 1000);
      };

      $scope.startImport = function() {
        $scope.statusMessage = "Uploading the file...";
        apiClient.createImport({
          "entity-name": $scope.entityName,
          "source-file": $scope.sourceFile
        }).then(function(response) {
          $scope.statusMessage = "Processing the file...";
          $scope.activeImport = response.data.import;
          trackImportProgress();
        });
      };
    }])
  .run(['$http', function($http) {
    $http.defaults.headers.common['Content-Type'] = "application/json";
    $http.defaults.headers.common['Accept'] = 'application/json';
  }]);
})();
