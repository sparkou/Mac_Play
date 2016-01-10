/**
 * OACtrl Controller
 */

var oa = angular.module('MSDash');
oa.controller('OACtrl', function($scope, $http) {
    $scope.templates = {
        home: '/assets/templates/oa/oahome.html',
        user: '/assets/templates/oa/oauser.html',
        leave: '/assets/templates/oa/oaleave.html',
        ot: '/assets/templates/oa/oaot.html',
        log: '/assets/templates/oa/oalog.html'
    }

    $scope.closeAlert = function() {
        $scope.alertShow = false;
    };
    showAlert = function(type, msg) {
        $scope.alertShow = true;
        $scope.alert = {type: type, msg: msg};
    }

    $http.get('/assets/data/user.json').success(function(data) {
        $scope.user = data[0];
    })

    $scope.isUserManage = false;
    $scope.manageUser = function() {
        $scope.isUserManage = !$scope.isUserManage;
    }

    $scope.okUser = function(user) {
        showAlert('info', 'User Successfully Updated!')
        $scope.user = user;
        $scope.isUserManage = !$scope.isUserManage;
    }
    $scope.cancelUser = function() {
        $scope.isUserManage = !$scope.isUserManage;
    }

    $scope.showUser = function() {
        alert('show user')
    }
});

