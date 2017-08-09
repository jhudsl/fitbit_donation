shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=\"' + name + '\"]');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
  console.log('running shinyjs stuff')
}


shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=\"' + name + '\"]');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}

console.log("initializing shinyjs functions")