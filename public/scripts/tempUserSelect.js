if (typeof jQuery == "function") {
	(function($) {
		$.fn.tempUserSelect = function(idType) {
			window.parent = $(this);
			window.newDiv = $(this).children('.new');
			window.select = $(this).children('select');
			window.select.change(function() {
				if($(window.select).val() == 'temporary') {
					window.newDiv.slideDown();
				} else {
					window.newDiv.slideUp();
				}
			});
			window.newDiv.children('button').click(function(){
				var celejmeno = window.newDiv.children('.jmeno').val() + ' ' + window.newDiv.children('.prijmeni').val();
				alert(celejmeno + ', ' + window.newDiv.children('.year').val());
				if(celejmeno == ' ') {
					return false;
				}
				window.select.children('option').each(function(){
					if($(this).text == celejmeno || $(this).text == (celejmeno + ', ' + window.newDiv.children('.year').val())) {
						window.newDiv.slideUp();
						window.select.val($(this).val());
						return false;
					}
				});
				$.ajax({
					type: 'POST',
					url: '/admin/users/temporary?ajax=ajax',
					data: {
						jmeno: window.newDiv.children('.jmeno').val(),
						prijmeni: window.newDiv.children('.prijmeni').val(),
						narozeni: (window.newDiv.children('.year').val() + '-' +
							window.newDiv.children('.month').val() + '-' + window.newDiv.children('.day').val())
					},
					beforeSend: function(){
						window.newDiv.slideUp();
						window.parent.children('.loading').slideDown();
					},
					success: function(data){
						if(typeof(data) != 'object') {
							data = JSON.parse(data);
						}
						var id = (idType == 'par') ? data.par_id : data.user_id;
						var fullname = data.jmeno + ' ' + data.prijmeni;
						
						window.select.append('<option value="' + id + '" selected="selected">' +
							fullname + ', ' + data.rok + '</option>');
						window.select.val(id);
						window.parent.children('.loading').slideUp();
					}
				});
				return false;
			});
		}
	})(jQuery);
}
