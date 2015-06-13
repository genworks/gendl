$(document).ready(function () { 

    $('.landing').show(); 

    $('.footer a').click(function () {
	$('.active').fadeOut(); 
	$('.active').removeClass('active'); 
	$('.'+$(this).data("toggle")).fadeIn().addClass('active'); 
    }); 

}); 