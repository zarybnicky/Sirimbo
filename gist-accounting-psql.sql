CREATE TABLE public.vouchers (
		id serial PRIMARY KEY,
		name text NOT NULL
);

INSERT INTO vouchers  
	(name)
VALUES
	('Sales'),
	('Purchase'),
	('Payment'),
	('Receipt'),
	('Contra'),
	('Journal'),
	('Credit Note'),
	('Debit Note');

CREATE TABLE transactions (
		id BIGSERIAL PRIMARY KEY,
		organization_id uuid NOT NULL REFERENCES organizations(id),
		voucher_id bigint NOT NULL REFERENCES vouchers(id),
		created_at timestamp without time zone DEFAULT now(),
		date date not null default current_date,
		description text COLLATE pg_catalog."default" NOT NULL,
		data jsonb
);

CREATE TABLE postings(
		id BIGSERIAL PRIMARY KEY,
		organization_id uuid NOT NULL REFERENCES organizations(id),
		ledger_id bigint NOT NULL REFERENCES ledgers(id),
		transaction_id bigint NOT NULL REFERENCES transactions(id) ON DELETE CASCADE,
		amount NUMERIC(20, 2) NOT NULL,
		is_credit boolean NOT NULL
);

CREATE VIEW normalized_postings as
select 
	postings.*,
	CASE 
		WHEN (ledgers.account_group_id IN (3,4,7,11,12,13) OR account_groups.parent_id IN (3,4,7,11,12,13)) THEN
			CASE
				WHEN postings.is_credit THEN
					postings.amount
				ELSE
					0.0 - postings.amount
			END
		ELSE
			CASE
				WHEN NOT postings.is_credit THEN
					postings.amount
				ELSE
					0.0 - postings.amount
			END
	END AS normalized_amount
from postings
JOIN ledgers ON ledgers.id = postings.ledger_id
JOIN account_groups ON ledgers.account_group_id = account_groups.id;

CREATE OR REPLACE FUNCTION public.update_closing_balance_for_change_of_ledgers()
		RETURNS trigger
		LANGUAGE 'plpgsql'
		COST 100
		VOLATILE NOT LEAKPROOF SECURITY DEFINER
AS $BODY$
	DECLARE 
		new_balance numeric;
BEGIN
	SELECT
		COALESCE(SUM(normalized_postings.normalized_amount), 0.0) into new_balance
	FROM
		normalized_postings
	JOIN ledgers ON ledgers.id = normalized_postings.ledger_id
	GROUP BY normalized_postings.ledger_id, ledgers.id
	HAVING normalized_postings.ledger_id = NEW.id;
	UPDATE public.ledgers
		SET closing_balance = opening_balance + COALESCE(new_balance, 0.0)
	WHERE ledgers.id = NEW.id;
	RETURN NULL;
END
$BODY$;

CREATE OR REPLACE FUNCTION public.update_closing_balance_for_change_of_postings()
		RETURNS trigger
		LANGUAGE 'plpgsql'
		COST 100
		VOLATILE NOT LEAKPROOF SECURITY DEFINER
AS $BODY$
	DECLARE 
		new_balance numeric;
BEGIN
	SELECT
		COALESCE(SUM(normalized_postings.normalized_amount), 0.0) into new_balance
	FROM
		normalized_postings
	JOIN ledgers ON ledgers.id = normalized_postings.ledger_id
	GROUP BY normalized_postings.ledger_id, ledgers.id
	HAVING normalized_postings.ledger_id = NEW.ledger_id;
	UPDATE public.ledgers
		SET closing_balance = opening_balance + COALESCE(new_balance, 0.0)
	WHERE ledgers.id = NEW.ledger_id;
	RETURN NULL;
END
$BODY$;

CREATE TRIGGER trigger_update_closing_balance_for_change_of_ledgers
	AFTER INSERT OR UPDATE OF opening_balance
	ON public.ledgers
	FOR EACH ROW
	EXECUTE PROCEDURE update_closing_balance_for_change_of_ledgers();

CREATE TRIGGER trigger_update_closing_balance_for_change_of_postings
	AFTER INSERT 
	OR UPDATE OF amount
	OR DELETE
	ON public.postings
	FOR EACH ROW
	EXECUTE PROCEDURE update_closing_balance_for_change_of_postings();

CREATE OR REPLACE FUNCTION insert_transaction(organization_id uuid, transaction_record json, journal_records json[])
RETURNS setof public.transactions AS $$
DECLARE
	new_transaction_id int;
	x json;
	new_journal_ids bigint[];
	newest_journal_id int;
	credit_total numeric;
	debit_total numeric;
BEGIN
	credit_total = 0;
	debit_total = 0;
	FOREACH x IN ARRAY journal_records LOOP
		IF (x->>'is_credit')::boolean is true
		THEN credit_total = credit_total + (x->>'amount')::numeric;
		ELSE debit_total = debit_total + (x->>'amount')::numeric;
		END IF;
	END LOOP;

	IF 
		credit_total != debit_total 
	THEN 
		RAISE EXCEPTION 'Credit amount must match debit amount';
	END IF;

	-- this check ensures to not enter a transaction if:
	-- the debit or credit is 0 or negative
	-- debit or credit entries are not specified for the transaction
	IF 
		credit_total < 1
	THEN 
		RAISE EXCEPTION 'Credit and Debit amount must be greater than 0';
	END IF;

	INSERT INTO public.transactions (organization_id, date, data, description, voucher_id) VALUES(
		organization_id,
		(transaction_record->>'date')::date,
		(transaction_record->>'data')::jsonb,
		(transaction_record->>'description')::text,
		(transaction_record->>'voucher_id')::bigint
	) RETURNING transactions.id INTO new_transaction_id;
	FOREACH x IN ARRAY journal_records LOOP

		INSERT INTO public.postings (organization_id, transaction_id, ledger_id, amount, is_credit) VALUES(
			organization_id,
			new_transaction_id,
			(x->>'ledger_id')::bigint,
			(x->>'amount')::numeric,
			(x->>'is_credit')::boolean
		) RETURNING postings.id INTO newest_journal_id;
		new_journal_ids := new_journal_ids || newest_journal_id;
	END LOOP;
	RETURN QUERY
	SELECT 
		*
	FROM 
		public.transactions
	WHERE 
		transactions.id = new_transaction_id;
END
$$ LANGUAGE plpgsql;


INSERT INTO account_groups
		(name, parent_id, icon)
VALUES
		('Branch/Division', null, 'mdi:source-branch'),
		('Current Assets', null, 'mdi:laptop'),
		('Indirect Income', null, 'mdi:cash'),
		('Capital Accounts', null, 'mdi:account-cash-outline'),
		('Fixed Assets', null, 'mdi:home-city'),
		('Misc. Expenses', null, 'mdi:folder'),
		('Loans (Liability)', null, 'mdi:cash-lock'),
		('Investments', null, 'mdi:coffee-maker'),
		('Purchase Accounts', null, 'mdi:cart-arrow-down'),
		('Suspense Account', null, 'mdi:alert-circle-outline'),
		('Sales Accounts', null, 'mdi:cart-arrow-up'),
		('Direct Income', null, 'mdi:cash-plus'),
		('Current Liabilities', null, 'mdi:cash-lock-open'),
		('Indirect Expenses', null, 'mdi:cash-refund'),
		('Direct Expenses', null, 'mdi:cash-minus'),
		('Sundry Creditors', 13, 'mdi:cash-lock-open'),
		('Secured Loans', 7, 'mdi:cash-lock'),
		('Bank Accounts', 2, 'mdi:bank'),
		('Stock in Hand', 2, 'mdi:bookshelf'),
		('Deposits', 2, 'mdi:cash-fast'),
		('Cash in Hand', 2, 'mdi:cash'),
		('Duties & Taxes', 13, 'mdi:cash-register'),
		('Banks OD Accounts', 7, 'mdi:bank-transfer-out'),
		('Loan & Advances (Assets)', 2, 'mdi:cash-100'),
		('Unsecured Loans', 7, 'mdi:cash-lock-open'),
		('Provisions', 13, 'mdi:cash-lock-open'),
		('Reserves & Surplus', 4, 'mdi:cash-lock-open'),
		('Sundry Debtors', 2, 'mdi:account-cash-outline'),
		('Parties', 2, 'mdi:account-tie'),
		('Lorry Suppliers', 13, 'mdi:card-account-details-outline'),
		('Drivers', 13, 'mdi:steering');

CREATE TABLE IF NOT EXISTS public.ledgers
(
		id BIGSERIAL PRIMARY KEY,
		created_at timestamp without time zone DEFAULT now(),
		maintain_balance_bill_by_bill boolean,
		name text COLLATE pg_catalog."default" NOT NULL,
		email email,
		organization_id uuid NOT NULL REFERENCES organizations(id),
		account_group_id bigint NOT NULL REFERENCES account_groups(id),
		opening_balance numeric(20, 2) NOT NULL DEFAULT 0.0,
		closing_balance numeric(20, 2),
		additional_fields jsonb,
		data jsonb,
		allow_transaction_refrence boolean
);
