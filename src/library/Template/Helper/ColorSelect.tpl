<script src='/scripts/spectrum.js' />
<link rel='stylesheet' href='/style/spectrum.css' />
<input id='color-{{ field }}' type='color' name='{{ field }}' value='{{ value }}' />
<script>
    $('#color-{{ field }}').spectrum(
    	{
            color:               '{{ value }}',
            showInput:           true,
            clickoutFiresChange: true,
            preferredFormat:     'hex6'
        }
    );
</script>