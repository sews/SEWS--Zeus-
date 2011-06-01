var show = function() {
  	if ($('#rightCol').not(':visible')) {
  		$('#rightCol').show();
		$('#centerCol').attr('class','narrow');
		}
	$('#searchresultsheading').show();
	if ($('.gsc-results').is(':visible')) {
		$('#searchresultsheading h2').css('color','#900');
		$('#searchresultsheading h2').html(lang=='sv'? 'S&ouml;kresultat':'Search result');
		$('#hide-button').show();
		}
		return false;
  };
  
var hide =  function() {
	  	if (!$('.gsc-results').is(':visible') && $('#rightColBox').length<1) {
  		$('#rightCol').hide();
		$('#centerCol').attr('class','wide');
		}
	if ($('.gsc-results').is(':visible')) {
		$('#searchresultsheading h2').css('color','#900');
		$('#searchresultsheading h2').html(lang=='sv'? 'S&ouml;kresultat':'Search result');
	}
	else {
		$('#searchresultsheading').hide();
		$('#hide-button').hide();
	}
		return false;
  };
	  
	  google.load('search', '1', {language : lang});
  google.setOnLoadCallback(function() {

    var customSearchControl = new google.search.CustomSearchControl(lang=='sv'? "002540261265754236040:_qhngrdmmhy": "002540261265754236040:dqwulcwc1xa");
    customSearchControl.setResultSetSize(google.search.Search.FILTERED_CSE_RESULTSET);
    customSearchControl.setLinkTarget(google.search.Search.LINK_TARGET_TOP);
    var options = new google.search.DrawOptions();
    options.setSearchFormRoot('cse-search-form');
    customSearchControl.draw('cse', options);
	  $('#cse-search-form .gsc-input').focusin(show);
	  $('#cse-search-form .gsc-input').focusout(hide);
	  $('#cse-search-form .gsc-search-button').focusin(show);
	  $('#cse-search-form .gsc-search-button').click(show);
	  $('#cse-search-form .gsc-search-button').focusout(hide);
	  $('#hide-button').click(function() {
		  $('#hide-button').hide();
		  if ($('#rightColBox').length<1) {
		  		$('#rightCol').hide();
				$('#centerCol').attr('class','wide');
				}
		  else {
		  $('#searchresultsheading').hide();
		  $('#cse').hide();
		  }
	  });
  }, true);