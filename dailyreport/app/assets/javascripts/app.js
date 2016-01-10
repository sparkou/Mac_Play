var reportApp = angular.module('reportApp', ['ui.router', 'ui.bootstrap']);


reportApp.config(function($stateProvider){
    $stateProvider
        .state('home', {
            url: "/home",
            templateUrl: "partials/home.html"
        })
});