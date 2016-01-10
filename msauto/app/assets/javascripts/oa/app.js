
var oa = angular.module('MSDash', ['ui.bootstrap', 'ui.router', 'ngCookies', 'pascalprecht.translate', 'ngSanitize']);

oa.config(['$stateProvider', '$urlRouterProvider',
    function($stateProvider, $urlRouterProvider) {

        $urlRouterProvider.otherwise('/');

        $stateProvider
            .state('index', {
                url: '/',
                templateUrl: '/assets/templates/dashboard.html'
            })
            .state('oa', {
                url: '/oa',
                templateUrl: '/assets/templates/oa/oa.html',
                controller: 'OACtrl'
            })
            .state('meeting', {
                url: '/meeting',
                templateUrl: '/assets/templates/meeting/meeting.html',
                controller: ''
            })
            .state('admin', {
                url: '/admin',
                templateUrl: '/assets/templates/admin.html',
                controller: 'AdminCtrl'
            })

    }
]);

oa.config(['$translateProvider', function ($translateProvider) {
    $translateProvider.useStaticFilesLoader({
        prefix: '/assets/i18n/',
        suffix: '.json'
    });
    // Tell the module what language to use by default
    $translateProvider.preferredLanguage('zh-cn');

    $translateProvider.useSanitizeValueStrategy('sanitizeParameters');
}]);