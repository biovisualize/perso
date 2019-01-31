(function() {
  "use strict";
  
  var init, setupWidgetCategoriesSelect, setupWidgetArchiveSelect, setupBottomWidgetCategoriesSelect, setupBottomWidgetArchiveSelect;

  init = function() {

    var i = 0;
    if (jQuery('.sidebar .widget_categories select').length) {
      jQuery('.sidebar .widget_categories select').each(function(index) {
        new Select({
          el: jQuery('.sidebar .widget_categories select')[i],
          alignToHighlighted: 'always'
        });
        i++;
      });
    }

    var i = 0;
    if (jQuery('.sidebar .widget_archive select').length) {
      jQuery('.sidebar .widget_archive select').each(function(index) {
        new Select({
          el: jQuery('.sidebar .widget_archive select')[i],
          alignToHighlighted: 'always'
        });
        i++;
      });
    }

    var i = 0;
    if (jQuery('.footer-section .widget_categories select').length) {
      jQuery('.footer-section .widget_categories select').each(function(index) {
        new Select({
          el: jQuery('.footer-section .widget_categories select')[i],
          alignToHighlighted: 'always'
        });
        i++;
      });
    }

    var i = 0;
    if (jQuery('.footer-section .widget_archive select').length) {
      jQuery('.footer-section .widget_archive select').each(function(index) {
        new Select({
          el: jQuery('.footer-section .widget_archive select')[i],
          alignToHighlighted: 'always'
        });
        i++;
      });
    }

    var i = 0;
    if (jQuery('.woocommerce-ordering select').length) {
      jQuery('.woocommerce-ordering select').each(function(index) {
        new Select({
          el: jQuery('.woocommerce-ordering select')[i],
          alignToHighlighted: 'always'
        });
        i++;
      });
    }
    var i = 0;
    if (jQuery('.wpcf7-select').length) {
      jQuery('.wpcf7-select').each(function(index) {
        new Select({
          el: jQuery('.wpcf7-select')[i],
          alignToHighlighted: 'always'
        });
        i++;
      });
    }

/*
    var i = 0;
    jQuery('select.country_to_state').each(function(index) {
      new Select({
        el: jQuery('select.country_to_state')[i],
        alignToHighlighted: 'always'
      });
      i++;
    });

    var i = 0;
    jQuery('select.state_select').each(function(index) {
      new Select({
        el: jQuery('select.state_select')[i],
        alignToHighlighted: 'always'
      });
      i++;
    });
*/
  };

  setupWidgetCategoriesSelect = function() {
    return new Select({
      el: jQuery('.sidebar .widget_categories select')[0],
      alignToHighlighted: 'always'
    });
  };

  setupWidgetArchiveSelect = function() {
    return new Select({
      el: jQuery('.sidebar .widget_archive select')[0],
      alignToHighlighted: 'always'
    });
  };

  setupBottomWidgetCategoriesSelect = function() {
    return new Select({
      el: jQuery('.footer-section .widget_categories select')[0],
      alignToHighlighted: 'always'
    });
  };

  setupBottomWidgetArchiveSelect = function() {
    return new Select({
      el: jQuery('.footer-section .widget_archive select')[0],
      alignToHighlighted: 'always'
    });
  };


  init();

}).call(this);
