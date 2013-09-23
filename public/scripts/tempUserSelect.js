if (typeof jQuery == "function") {
	(function($) {
		$.fn.tempUserSelect = function(idType) {
			$this = $(this);
			$newDiv = $this.children('.new');
			$select = $this.children('select');
			$select.attr('type', idType);
			$select.change($.proxy(function() {
				$this = $(this);
				if($this.children('select').val() == 'temporary') {
					$this.children('.new').slideDown();
				} else {
					$this.children('.new').slideUp();
				}
			}, this));
			$newDiv.children('button').click($.proxy(function() {
				$this = $(this);
				$newDiv = $this.children('.new');
				$select = $this.children('select');
				$loading = $this.children('.loading');
				
				var celejmeno = $newDiv.children('.jmeno').val() + ' ' + $newDiv.children('.prijmeni').val();
				if(celejmeno == ' ') {
					return false;
				}
				$.ajax({
					type: 'POST',
					url: '/admin/users/temporary?ajax=ajax',
					data: {
						type: $select.attr('type'),
						jmeno: $newDiv.children('input[name=jmeno]').val(),
						prijmeni: $newDiv.children('input[name=prijmeni]').val(),
						narozeni: $newDiv.children('input[name=narozeni]').val()
					},
					beforeSend: $.proxy(function() {
						$this = $(this);
						$newDiv = $this.children('.new');
						$select = $this.children('select');
						
						$newDiv.slideUp();
						$loading.slideDown();
					}, this),
					success: $.proxy(function(data) {
						$this = $(this);
						$select = $this.children('select');
						$loading = $this.children('.loading');
						
						if(typeof(data) != 'object') {
							data = JSON.parse(data);
						}
						var id = ($select.attr('type') == 'par') ? data.par_id : data.user_id;
						var fullname = data.jmeno + ' ' + data.prijmeni;
						
						$select.children('option').each(function() {
							$this = $(this);
							if($this.val() == id) {
								$newDiv = $this.parent().parent().children('.new');
								$select = $this.parent().parent().children('select');

								$loading.slideUp();
								$select.val($this.val());
								return true;
							}
						});
						if($select.val() == id)
							return true;
						
						$select.append('<option value="' + id + '" selected="selected">' +
							fullname + ', ' + data.rok + '</option>');
						$select.val(id);
						$loading.slideUp();
					}, this)
				});
				return false;
			}, this));
		};
	})(jQuery);
}
