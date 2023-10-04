CREATE DOMAIN public.price AS public.price_type
	CONSTRAINT price_check CHECK (((VALUE IS NULL) OR (((VALUE).currency IS NOT NULL) AND ((VALUE).amount IS NOT NULL) AND (length((VALUE).currency) = 3) AND ((VALUE).currency = upper((VALUE).currency)))));



