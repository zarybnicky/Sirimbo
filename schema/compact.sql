CREATE TYPE public.gender_type AS ENUM ('man', 'woman', 'unspecified');

CREATE TYPE public.address_type AS (street text, conscription_number text, orientation_number text, district text, city text, region text, postal_code text);

CREATE DOMAIN public.address_domain AS public.address_type CONSTRAINT address_domain_check
  CHECK (
  value IS NULL
    OR ((value).street IS NOT NULL
    AND (value).conscription_number IS NOT NULL
    AND (value).orientation_number IS NOT NULL
    AND (value).city IS NOT NULL
    AND (value).region IS NOT NULL
    AND (value).postal_code IS NOT NULL)
);

CREATE TABLE public.person (
  id bigint NOT NULL PRIMARY KEY,
  first_name text NOT NULL,
  last_name text NOT NULL,
  gender public.gender_type NOT NULL,
  birth_date date,
  nationality text NOT NULL,
  tax_identification_number text,
  national_id_number text,
  csts_id text,
  wdsf_id text,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  legacy_user_id bigint,
  prefix_title text DEFAULT ''::text NOT NULL,
  suffix_title text DEFAULT ''::text NOT NULL,
  bio text DEFAULT ''::text NOT NULL,
  email public.citext,
  phone text,
  name text GENERATED ALWAYS AS (public.immutable_concat_ws(' '::text, VARIADIC ARRAY[NULLIF (TRIM(BOTH FROM prefix_title), ''::text), NULLIF (TRIM(BOTH FROM first_name), ''::text), NULLIF (TRIM(BOTH FROM last_name), ''::text), CASE
    WHEN suffix_title IS NULL
      OR TRIM(BOTH FROM suffix_title) = ''::text THEN NULL::text
    ELSE public.immutable_concat_ws(' '::text, VARIADIC ARRAY[','::text, TRIM(BOTH FROM suffix_title)])
  END])) STORED NOT NULL,
  address public.address_domain,
  external_ids text[]
);

CREATE TYPE public.relationship_status AS ENUM ('pending', 'active', 'expired');

CREATE TABLE public.couple (
  id bigint NOT NULL PRIMARY KEY,
  man_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  woman_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  since timestamp with time zone DEFAULT now() NOT NULL,
  until timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  legacy_pary_id bigint,
  active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
  status public.relationship_status DEFAULT CAST('active' AS public.relationship_status) NOT NULL,
  EXCLUDE USING gist (man_id WITH =, woman_id WITH =, active_range WITH &&)
);

CREATE TABLE public.tenant (
  id bigint NOT NULL PRIMARY KEY,
  name text NOT NULL,
  origins text[] DEFAULT CAST(ARRAY[] AS text[]) NOT NULL,
  cz_ico text DEFAULT ''::text NOT NULL,
  cz_dic text DEFAULT ''::text NOT NULL,
  address public.address_domain,
  description text DEFAULT ''::text NOT NULL,
  bank_account text DEFAULT ''::text NOT NULL
);

CREATE TABLE public.account (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  opening_balance numeric(19, 4) DEFAULT 0.0 NOT NULL,
  currency public.citext DEFAULT CAST('CZK' AS public.citext) NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  UNIQUE NULLS NOT DISTINCT (tenant_id, person_id, currency)
);

CREATE TABLE public.accounting_period (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  name text DEFAULT ''::text NOT NULL,
  since timestamp with time zone NOT NULL,
  until timestamp with time zone NOT NULL,
  range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  EXCLUDE USING gist (tenant_id WITH =, range WITH &&)
);

CREATE TABLE public.cohort_group (
  id bigint NOT NULL PRIMARY KEY,
  name text NOT NULL,
  description text DEFAULT ''::text NOT NULL,
  ordering int DEFAULT 1 NOT NULL,
  is_public boolean DEFAULT true NOT NULL,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE public.cohort (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  cohort_group_id bigint REFERENCES public.cohort_group (id)
    ON DELETE SET NULL,
  name text NOT NULL,
  description text DEFAULT ''::text NOT NULL,
  color_rgb text NOT NULL,
  location text DEFAULT ''::text NOT NULL,
  is_visible boolean DEFAULT true NOT NULL,
  ordering int DEFAULT 1 NOT NULL,
  external_ids text[]
);

CREATE TABLE public.cohort_membership (
  cohort_id bigint NOT NULL REFERENCES public.cohort (id),
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  since timestamp with time zone DEFAULT now() NOT NULL,
  until timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
  status public.relationship_status DEFAULT CAST('active' AS public.relationship_status) NOT NULL,
  EXCLUDE USING gist (cohort_id WITH =, person_id WITH =, active_range WITH &&)
);

CREATE TYPE public.price_type AS (amount numeric(19, 4), currency text);

CREATE DOMAIN public.price AS public.price_type CONSTRAINT price_check
  CHECK (
  value IS NULL
    OR ((value).currency IS NOT NULL
    AND (value).amount IS NOT NULL
    AND length((value).currency) = 3
    AND (value).currency = upper((value).currency))
);

CREATE TABLE public.cohort_subscription (
  id bigint NOT NULL PRIMARY KEY,
  cohort_id bigint NOT NULL REFERENCES public.cohort (id),
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  account_id bigint NOT NULL REFERENCES public.account (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  price public.price NOT NULL,
  active boolean DEFAULT true NOT NULL,
  renews_on timestamp with time zone,
  interval interval DEFAULT '1 mon'::interval NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.form_responses (
  id bigint NOT NULL PRIMARY KEY,
  type text NOT NULL,
  data jsonb NOT NULL,
  url text NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE public.person_invitation (
  id bigint NOT NULL PRIMARY KEY,
  access_token uuid DEFAULT gen_random_uuid() NOT NULL UNIQUE,
  person_id bigint REFERENCES public.person (id)
    ON DELETE CASCADE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  used_at timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  email public.citext NOT NULL
);

CREATE TABLE public.scoreboard_manual_adjustment (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id),
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  cohort_id bigint REFERENCES public.cohort (id),
  points int NOT NULL,
  reason text,
  awarded_at date DEFAULT CURRENT_DATE NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.tenant_administrator (
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  since timestamp with time zone DEFAULT now() NOT NULL,
  until timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  id bigint NOT NULL PRIMARY KEY,
  is_visible boolean DEFAULT true NOT NULL,
  description text DEFAULT ''::text NOT NULL,
  active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
  status public.relationship_status DEFAULT CAST('active' AS public.relationship_status) NOT NULL,
  EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&)
);

CREATE TABLE public.tenant_location (
  id bigint NOT NULL PRIMARY KEY,
  name text NOT NULL,
  description text DEFAULT ''::text NOT NULL,
  address public.address_domain,
  is_public boolean DEFAULT true,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id),
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TYPE public.event_type AS ENUM ('camp', 'lesson', 'reservation', 'holiday', 'group');

CREATE TYPE public.event_payment_type AS ENUM ('upfront', 'after_instance', 'none');

CREATE TABLE public.event (
  id bigint NOT NULL PRIMARY KEY,
  name text NOT NULL,
  location_text text NOT NULL,
  description text NOT NULL,
  since date,
  until date,
  capacity int DEFAULT CAST('0' AS bigint) NOT NULL,
  files_legacy text DEFAULT ''::text NOT NULL,
  updated_at timestamp with time zone,
  is_locked boolean DEFAULT false NOT NULL,
  is_visible boolean DEFAULT false NOT NULL,
  summary text DEFAULT ''::text NOT NULL,
  is_public boolean DEFAULT false NOT NULL,
  enable_notes boolean DEFAULT false NOT NULL,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  description_member text DEFAULT ''::text NOT NULL,
  title_image_legacy text,
  type public.event_type DEFAULT CAST('camp' AS public.event_type) NOT NULL,
  location_id bigint REFERENCES public.tenant_location (id),
  payment_type public.event_payment_type DEFAULT CAST('none' AS public.event_payment_type) NOT NULL,
  is_paid_by_tenant boolean DEFAULT true NOT NULL,
  member_price public.price DEFAULT CAST(NULL AS public.price_type),
  guest_price public.price DEFAULT CAST(NULL AS public.price_type),
  payment_recipient_id bigint REFERENCES public.account (id),
  created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE public.event_instance (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  event_id bigint NOT NULL REFERENCES public.event (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  since timestamp with time zone NOT NULL,
  until timestamp with time zone NOT NULL,
  location_id bigint REFERENCES public.tenant_location (id),
  is_cancelled boolean DEFAULT false,
  range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL
);

CREATE TABLE public.event_instance_trainer (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  instance_id bigint NOT NULL REFERENCES public.event_instance (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  lesson_price public.price DEFAULT CAST(NULL AS public.price_type),
  UNIQUE (instance_id, person_id)
);

CREATE TABLE public.event_target_cohort (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  event_id bigint NOT NULL REFERENCES public.event (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  cohort_id bigint NOT NULL REFERENCES public.cohort (id),
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  UNIQUE (event_id, cohort_id)
);

CREATE TABLE public.event_registration (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  event_id bigint NOT NULL REFERENCES public.event (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  target_cohort_id bigint REFERENCES public.event_target_cohort (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  couple_id bigint REFERENCES public.couple (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  note text,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  CHECK (
    (couple_id IS NOT NULL
      AND person_id IS NULL)
      OR (couple_id IS NULL
      AND person_id IS NOT NULL)
  ),
  UNIQUE NULLS NOT DISTINCT (event_id, person_id, couple_id)
);

CREATE TYPE public.attendance_type AS ENUM ('unknown', 'attended', 'not-excused', 'cancelled');

CREATE TABLE public.event_attendance (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  instance_id bigint NOT NULL REFERENCES public.event_instance (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  status public.attendance_type DEFAULT CAST('unknown' AS public.attendance_type) NOT NULL,
  note text,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  registration_id bigint NOT NULL REFERENCES public.event_registration (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  UNIQUE (registration_id, instance_id, person_id)
);

CREATE TABLE public.event_trainer (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  event_id bigint NOT NULL REFERENCES public.event (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  lessons_offered int DEFAULT 0 NOT NULL,
  lesson_price public.price DEFAULT CAST(NULL AS public.price_type),
  UNIQUE (event_id, person_id)
);

CREATE TABLE public.event_lesson_demand (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  trainer_id bigint NOT NULL REFERENCES public.event_trainer (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  registration_id bigint NOT NULL REFERENCES public.event_registration (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  lesson_count int NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  CHECK (lesson_count > 0),
  UNIQUE (registration_id, trainer_id)
);

CREATE TYPE public.payment_status AS ENUM ('tentative', 'unpaid', 'paid');

CREATE TABLE public.payment (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  accounting_period_id bigint NOT NULL REFERENCES public.accounting_period (id),
  cohort_subscription_id bigint REFERENCES public.cohort_subscription (id)
    ON UPDATE CASCADE
    ON DELETE SET NULL,
  event_registration_id bigint REFERENCES public.event_registration (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  event_instance_id bigint REFERENCES public.event_instance (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  status public.payment_status NOT NULL,
  variable_symbol text,
  specific_symbol text,
  is_auto_credit_allowed boolean DEFAULT true NOT NULL,
  tags text[] DEFAULT CAST(ARRAY[] AS text[]) NOT NULL,
  due_at timestamp with time zone,
  paid_at timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.payment_debtor (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  payment_id bigint NOT NULL REFERENCES public.payment (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE
);

CREATE TABLE public.payment_recipient (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  payment_id bigint NOT NULL REFERENCES public.payment (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  account_id bigint NOT NULL REFERENCES public.account (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  amount numeric(19, 4) NOT NULL
);

CREATE TABLE public.tenant_membership (
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  since timestamp with time zone DEFAULT now() NOT NULL,
  until timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  id bigint NOT NULL PRIMARY KEY,
  active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
  status public.relationship_status DEFAULT CAST('active' AS public.relationship_status) NOT NULL,
  EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&)
);

CREATE TABLE public.tenant_settings (
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL PRIMARY KEY REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  settings jsonb NOT NULL
);

CREATE TABLE public.tenant_trainer (
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  since timestamp with time zone DEFAULT now() NOT NULL,
  until timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  id bigint NOT NULL PRIMARY KEY,
  is_visible boolean DEFAULT true,
  description text DEFAULT ''::text NOT NULL,
  active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
  member_price_45min public.price DEFAULT CAST(NULL AS public.price_type),
  member_payout_45min public.price DEFAULT CAST(NULL AS public.price_type),
  guest_price_45min public.price DEFAULT CAST(NULL AS public.price_type),
  guest_payout_45min public.price DEFAULT CAST(NULL AS public.price_type),
  create_payout_payments boolean DEFAULT true NOT NULL,
  status public.relationship_status DEFAULT CAST('active' AS public.relationship_status) NOT NULL,
  EXCLUDE USING gist (tenant_id WITH =, person_id WITH =, active_range WITH &&)
);

CREATE TYPE public.transaction_source AS ENUM ('auto-bank', 'auto-credit', 'manual-bank', 'manual-credit', 'manual-cash');

CREATE TABLE public.transaction (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  accounting_period_id bigint NOT NULL REFERENCES public.accounting_period (id),
  payment_id bigint REFERENCES public.payment (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  source public.transaction_source NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  description text,
  effective_date timestamp with time zone NOT NULL
);

CREATE TABLE public.posting (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  transaction_id bigint NOT NULL REFERENCES public.transaction (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  account_id bigint NOT NULL REFERENCES public.account (id),
  original_account_id bigint REFERENCES public.account (id),
  amount numeric(19, 4),
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.users (
  id bigint NOT NULL PRIMARY KEY,
  u_login public.citext,
  u_pass char(40) NOT NULL,
  u_jmeno text,
  u_prijmeni text,
  u_email public.citext NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  u_ban boolean DEFAULT true NOT NULL,
  u_confirmed boolean DEFAULT false NOT NULL,
  u_created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  last_login timestamp with time zone,
  created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
  last_active_at timestamp with time zone,
  last_version text
);

CREATE TABLE public.announcement (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  author_id bigint REFERENCES public.users (id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  title text NOT NULL,
  body text NOT NULL,
  is_locked boolean DEFAULT false NOT NULL,
  is_visible boolean DEFAULT true NOT NULL,
  is_sticky boolean DEFAULT false NOT NULL,
  created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
  updated_at timestamp with time zone,
  scheduled_since timestamp with time zone,
  scheduled_until timestamp with time zone
);

CREATE TYPE public.announcement_audience_role AS ENUM ('member', 'trainer', 'administrator');

CREATE TABLE public.announcement_audience (
  id bigint NOT NULL PRIMARY KEY,
  announcement_id bigint NOT NULL REFERENCES public.announcement (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  cohort_id bigint REFERENCES public.cohort (id),
  audience_role public.announcement_audience_role,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  CHECK ((cohort_id IS NULL) <> (audience_role IS NULL))
);

CREATE TABLE public.attachment (
  object_name text NOT NULL PRIMARY KEY,
  preview_object_name text,
  uploaded_by bigint DEFAULT public.current_user_id() REFERENCES public.users (id)
    ON DELETE SET NULL,
  uploaded_at timestamp with time zone DEFAULT now() NOT NULL,
  thumbhash text,
  width int,
  height int
);

CREATE TABLE public.dokumenty (
  id bigint NOT NULL PRIMARY KEY,
  d_path text NOT NULL,
  d_name text NOT NULL,
  d_filename text NOT NULL,
  d_kategorie smallint NOT NULL,
  d_kdo bigint NOT NULL REFERENCES public.users (id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
  d_timestamp timestamp with time zone GENERATED ALWAYS AS (updated_at) STORED
);

CREATE TABLE public.event_external_registration (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  event_id bigint NOT NULL REFERENCES public.event (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  first_name text NOT NULL,
  last_name text NOT NULL,
  prefix_title text DEFAULT ''::text NOT NULL,
  suffix_title text DEFAULT ''::text NOT NULL,
  nationality text NOT NULL,
  birth_date date,
  tax_identification_number text,
  email public.citext NOT NULL,
  phone text NOT NULL,
  note text,
  created_by bigint DEFAULT public.current_user_id() REFERENCES public.users (id),
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TYPE public.application_form_status AS ENUM ('new', 'sent', 'approved', 'rejected');

CREATE TABLE public.membership_application (
  id bigint NOT NULL PRIMARY KEY,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  first_name text NOT NULL,
  middle_name text,
  last_name text NOT NULL,
  gender public.gender_type NOT NULL,
  birth_date date,
  nationality text NOT NULL,
  tax_identification_number text,
  national_id_number text,
  csts_id text,
  wdsf_id text,
  prefix_title text,
  suffix_title text,
  bio text,
  email public.citext,
  phone text,
  created_by bigint NOT NULL REFERENCES public.users (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  status public.application_form_status DEFAULT CAST('sent' AS public.application_form_status) NOT NULL,
  note text DEFAULT ''::text NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.otp_token (
  id bigint NOT NULL PRIMARY KEY,
  access_token uuid DEFAULT gen_random_uuid() NOT NULL UNIQUE,
  user_id bigint REFERENCES public.users (id)
    ON DELETE CASCADE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  expires_at timestamp with time zone DEFAULT now() + '24:00:00'::interval NOT NULL,
  used_at timestamp with time zone,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE public.user_proxy (
  user_id bigint NOT NULL REFERENCES public.users (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  person_id bigint NOT NULL REFERENCES public.person (id)
    ON UPDATE CASCADE
    ON DELETE CASCADE,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  updated_at timestamp with time zone DEFAULT now() NOT NULL,
  id bigint NOT NULL PRIMARY KEY,
  since timestamp with time zone,
  until timestamp with time zone,
  active_range tstzrange GENERATED ALWAYS AS (tstzrange(since, until, '[)'::text)) STORED NOT NULL,
  status public.relationship_status DEFAULT CAST('active' AS public.relationship_status) NOT NULL,
  EXCLUDE USING gist (user_id WITH =, person_id WITH =, active_range WITH &&)
);

CREATE TYPE federated.competitor_type AS ENUM ('couple', 'solo', 'duo', 'formation', 'team');

CREATE TABLE federated.competitor (
  id bigint NOT NULL PRIMARY KEY,
  competitor_type federated.competitor_type NOT NULL,
  name text,
  created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE federated.dance (
  code text NOT NULL PRIMARY KEY,
  name text NOT NULL,
  discipline text NOT NULL
);

CREATE TABLE federated.dance_program (
  id bigint NOT NULL PRIMARY KEY,
  code text UNIQUE,
  name text NOT NULL,
  discipline text,
  is_default boolean DEFAULT false NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE federated.category (
  id bigint NOT NULL PRIMARY KEY,
  name text NOT NULL,
  series text NOT NULL,
  discipline text NOT NULL,
  age_group text NOT NULL,
  gender_group text DEFAULT 'mixed'::text NOT NULL,
  class text NOT NULL,
  base_dance_program_id bigint REFERENCES federated.dance_program (id),
  UNIQUE (series, discipline, age_group, gender_group, class)
);

CREATE TABLE federated.dance_program_dance (
  program_id bigint NOT NULL REFERENCES federated.dance_program (id)
    ON DELETE CASCADE,
  dance_code text NOT NULL REFERENCES federated.dance (code),
  dance_order int NOT NULL,
  PRIMARY KEY (program_id, dance_code),
  UNIQUE (program_id, dance_order)
);

CREATE TABLE federated.federation (
  code text NOT NULL PRIMARY KEY,
  name text NOT NULL
);

CREATE TABLE federated.competitor_category_progress (
  federation text NOT NULL REFERENCES federated.federation (code),
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  category_id bigint NOT NULL REFERENCES federated.category (id),
  points numeric(10, 3) DEFAULT 0 NOT NULL,
  domestic_finale int DEFAULT 0 NOT NULL,
  foreign_finale int DEFAULT 0 NOT NULL,
  PRIMARY KEY (federation, competitor_id, category_id)
);

CREATE TABLE federated.federation_category (
  federation text NOT NULL REFERENCES federated.federation (code),
  external_id text,
  category_id bigint NOT NULL REFERENCES federated.category (id),
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  UNIQUE (federation, external_id),
  PRIMARY KEY (federation, category_id)
);

CREATE TABLE federated.federation_club (
  id bigint NOT NULL PRIMARY KEY,
  federation text NOT NULL REFERENCES federated.federation (code),
  external_id text NOT NULL,
  name text NOT NULL,
  city text,
  country text,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  UNIQUE (federation, external_id)
);

CREATE TABLE federated.competitor_club_affiliation (
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  club_id bigint NOT NULL REFERENCES federated.federation_club (id),
  valid_from date NOT NULL,
  valid_to date,
  CHECK (
    valid_to IS NULL
      OR valid_to >= valid_from
  ),
  EXCLUDE USING gist (competitor_id WITH =, club_id WITH =, (daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[]'::text)) WITH &&),
  PRIMARY KEY (competitor_id, club_id, valid_from)
);

CREATE TABLE federated.event (
  id bigint NOT NULL PRIMARY KEY,
  federation text NOT NULL REFERENCES federated.federation (code),
  external_id text NOT NULL,
  name text,
  start_date date NOT NULL,
  end_date date,
  location text,
  country text,
  organizing_club_id bigint REFERENCES federated.federation_club (id),
  range daterange GENERATED ALWAYS AS (CASE
    WHEN end_date IS NULL THEN daterange(start_date, start_date, '[]'::text)
    ELSE daterange(start_date, end_date, '[]'::text)
  END) STORED,
  CHECK (
    end_date IS NULL
      OR end_date >= start_date
  ),
  UNIQUE (federation, external_id)
);

CREATE TABLE federated.competition (
  id bigint NOT NULL PRIMARY KEY,
  federation text NOT NULL REFERENCES federated.federation (code),
  external_id text NOT NULL,
  event_id bigint NOT NULL REFERENCES federated.event (id),
  category_id bigint NOT NULL REFERENCES federated.category (id),
  start_date date,
  end_date date,
  CHECK (
    end_date IS NULL
      OR end_date >= start_date
  ),
  UNIQUE (federation, external_id)
);

CREATE TABLE federated.competition_entry (
  competition_id bigint NOT NULL REFERENCES federated.competition (id),
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  cancelled boolean DEFAULT false NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  PRIMARY KEY (competition_id, competitor_id)
);

CREATE TABLE federated.competition_result (
  competition_id bigint NOT NULL REFERENCES federated.competition (id),
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  start_number text,
  ranking int NOT NULL,
  ranking_to int,
  point_gain numeric(10, 3),
  final_gain numeric(10, 3),
  CHECK (
    ranking_to IS NULL
      OR ranking_to >= ranking
  ),
  UNIQUE (competition_id, start_number),
  PRIMARY KEY (competition_id, competitor_id)
);

CREATE TYPE federated.scoring_method AS ENUM ('skating_marks', 'skating_places', 'ajs-3.0');

CREATE TABLE federated.competition_round (
  id bigint NOT NULL PRIMARY KEY,
  competition_id bigint NOT NULL REFERENCES federated.competition (id),
  round_index int NOT NULL,
  round_type text,
  dance_program_id bigint NOT NULL REFERENCES federated.dance_program (id),
  scoring_method federated.scoring_method NOT NULL,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  UNIQUE (competition_id, round_index)
);

CREATE TABLE federated.competition_round_result (
  round_id bigint NOT NULL REFERENCES federated.competition_round (id),
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  overall_ranking int NOT NULL,
  overall_ranking_to int,
  qualified_next boolean,
  overall_score numeric(10, 3),
  CHECK (
    overall_ranking_to IS NULL
      OR overall_ranking_to >= overall_ranking
  ),
  PRIMARY KEY (round_id, competitor_id)
);

CREATE TABLE federated.federation_competitor (
  federation text NOT NULL REFERENCES federated.federation (code),
  external_id text NOT NULL,
  competitor_id bigint REFERENCES federated.competitor (id),
  age_group text,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  UNIQUE (federation, competitor_id),
  PRIMARY KEY (federation, external_id)
);

CREATE TYPE federated.gender AS ENUM ('male', 'female', 'other', 'unknown');

CREATE TABLE federated.person (
  id bigint NOT NULL PRIMARY KEY,
  canonical_name text,
  first_name text,
  last_name text,
  search_name text GENERATED ALWAYS AS (federated.unaccent(COALESCE(canonical_name, (first_name || ' '::text) || last_name))) STORED,
  gender federated.gender,
  dob date,
  nationality text,
  created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE federated.athlete (
  id bigint NOT NULL PRIMARY KEY,
  person_id bigint NOT NULL UNIQUE REFERENCES federated.person (id),
  created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE federated.athlete_club_membership (
  athlete_id bigint NOT NULL REFERENCES federated.athlete (id),
  club_id bigint NOT NULL REFERENCES federated.federation_club (id),
  valid_from date NOT NULL,
  valid_to date,
  CHECK (
    valid_to IS NULL
      OR valid_to >= valid_from
  ),
  EXCLUDE USING gist (athlete_id WITH =, club_id WITH =, (daterange(valid_from, COALESCE(valid_to, 'infinity'::date), '[]'::text)) WITH &&),
  PRIMARY KEY (athlete_id, club_id, valid_from)
);

CREATE TYPE federated.competitor_role AS ENUM ('lead', 'follow', 'member', 'substitute');

CREATE TABLE federated.competitor_component (
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  athlete_id bigint NOT NULL REFERENCES federated.athlete (id),
  role federated.competitor_role NOT NULL,
  PRIMARY KEY (competitor_id, athlete_id)
);

CREATE TABLE federated.federation_athlete (
  federation text NOT NULL REFERENCES federated.federation (code),
  external_id text NOT NULL,
  athlete_id bigint REFERENCES federated.athlete (id),
  age_group text,
  medical_checkup_expiration date,
  medical_checkup_type text,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  PRIMARY KEY (federation, external_id)
);

CREATE TABLE federated.judge (
  id bigint NOT NULL PRIMARY KEY,
  person_id bigint NOT NULL UNIQUE REFERENCES federated.person (id),
  created_at timestamp with time zone DEFAULT now() NOT NULL
);

CREATE TABLE federated.federation_judge (
  federation text NOT NULL REFERENCES federated.federation (code),
  external_id text NOT NULL,
  judge_id bigint REFERENCES federated.judge (id),
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  PRIMARY KEY (federation, external_id)
);

CREATE TABLE federated.ranklist (
  id bigint NOT NULL PRIMARY KEY,
  federation text NOT NULL REFERENCES federated.federation (code),
  category_id bigint NOT NULL REFERENCES federated.category (id),
  name text NOT NULL,
  UNIQUE (federation, category_id)
);

CREATE TABLE federated.ranklist_snapshot (
  id bigint NOT NULL PRIMARY KEY,
  ranklist_id bigint NOT NULL REFERENCES federated.ranklist (id),
  kind text DEFAULT 'default'::text NOT NULL,
  as_of_date date NOT NULL,
  UNIQUE (ranklist_id, as_of_date, kind)
);

CREATE TABLE federated.ranklist_entry (
  snapshot_id bigint NOT NULL REFERENCES federated.ranklist_snapshot (id),
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  ranking int NOT NULL,
  ranking_to int,
  points numeric(10, 3),
  CHECK (
    ranking_to IS NULL
      OR ranking_to >= ranking
  ),
  UNIQUE (snapshot_id, competitor_id)
);

CREATE TABLE federated.round_dance (
  id bigint NOT NULL PRIMARY KEY,
  round_id bigint NOT NULL REFERENCES federated.competition_round (id),
  dance_code text NOT NULL REFERENCES federated.dance (code),
  UNIQUE (round_id, dance_code)
);

CREATE TYPE federated.score_component AS ENUM ('mark', 'places', 'ajs_tq', 'ajs_mm', 'ajs_ps', 'ajs_cp');

CREATE TABLE federated.judge_score (
  round_dance_id bigint NOT NULL REFERENCES federated.round_dance (id),
  judge_id bigint NOT NULL REFERENCES federated.judge (id),
  competitor_id bigint NOT NULL REFERENCES federated.competitor (id),
  component federated.score_component NOT NULL,
  score numeric(10, 3) NOT NULL,
  raw_score text,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  PRIMARY KEY (round_dance_id, judge_id, competitor_id, component)
);

CREATE TABLE federated.round_judge (
  round_id bigint NOT NULL REFERENCES federated.competition_round (id),
  judge_id bigint NOT NULL REFERENCES federated.judge (id),
  is_shadow boolean DEFAULT false NOT NULL,
  PRIMARY KEY (round_id, judge_id)
);

CREATE TABLE app_private.galerie_dir (
  id bigint NOT NULL PRIMARY KEY,
  gd_id_rodic bigint NOT NULL,
  gd_name text NOT NULL,
  gd_level smallint DEFAULT CAST('1' AS smallint) NOT NULL,
  gd_path text NOT NULL,
  gd_hidden boolean DEFAULT true NOT NULL,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE public.galerie_foto (
  id bigint NOT NULL PRIMARY KEY,
  gf_id_rodic bigint NOT NULL REFERENCES app_private.galerie_dir (id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  gf_name text NOT NULL,
  gf_path text NOT NULL,
  gf_kdo bigint NOT NULL REFERENCES public.users (id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  updated_at timestamp with time zone,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE public.aktuality (
  id bigint NOT NULL PRIMARY KEY,
  at_kdo bigint REFERENCES public.users (id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  at_kat text DEFAULT '1'::text NOT NULL,
  at_jmeno text NOT NULL,
  at_text text NOT NULL,
  at_preview text NOT NULL,
  at_foto bigint,
  at_foto_main bigint REFERENCES public.galerie_foto (id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  updated_at timestamp with time zone,
  created_at timestamp with time zone DEFAULT now(),
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  title_photo_url text
);

CREATE TABLE app_private.platby_category (
  pc_id bigint NOT NULL PRIMARY KEY,
  pc_name text NOT NULL,
  pc_symbol bigint NOT NULL,
  pc_amount numeric(10, 2) NOT NULL,
  pc_date_due date NOT NULL,
  pc_valid_from date NOT NULL,
  pc_valid_to date NOT NULL,
  pc_use_base boolean DEFAULT false NOT NULL,
  pc_use_prefix boolean DEFAULT false NOT NULL,
  pc_archive boolean DEFAULT false NOT NULL,
  pc_visible boolean DEFAULT true NOT NULL,
  id bigint GENERATED ALWAYS AS (pc_id) STORED NOT NULL UNIQUE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE app_private.platby_group (
  pg_id bigint NOT NULL PRIMARY KEY,
  pg_type numeric DEFAULT '1'::numeric NOT NULL,
  pg_name text NOT NULL,
  pg_description text NOT NULL,
  pg_base bigint DEFAULT CAST('0' AS bigint) NOT NULL,
  id bigint GENERATED ALWAYS AS (pg_id) STORED NOT NULL UNIQUE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE app_private.platby_category_group (
  pcg_id bigint NOT NULL PRIMARY KEY,
  pcg_id_group bigint NOT NULL REFERENCES app_private.platby_group (pg_id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  pcg_id_category bigint NOT NULL REFERENCES app_private.platby_category (pc_id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  id bigint GENERATED ALWAYS AS (pcg_id) STORED NOT NULL UNIQUE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE app_private.platby_group_skupina (
  pgs_id bigint NOT NULL PRIMARY KEY,
  pgs_id_skupina bigint NOT NULL REFERENCES public.cohort (id),
  pgs_id_group bigint NOT NULL REFERENCES app_private.platby_group (pg_id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  id bigint GENERATED ALWAYS AS (pgs_id) STORED NOT NULL UNIQUE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE app_private.platby_raw (
  pr_id bigint NOT NULL PRIMARY KEY,
  pr_raw bytea NOT NULL,
  pr_hash text NOT NULL,
  pr_sorted boolean DEFAULT true NOT NULL,
  pr_discarded boolean DEFAULT true NOT NULL,
  id bigint GENERATED ALWAYS AS (pr_id) STORED NOT NULL UNIQUE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE
);

CREATE TABLE app_private.platby_item (
  pi_id bigint NOT NULL PRIMARY KEY,
  pi_id_user bigint REFERENCES public.users (id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  pi_id_category bigint NOT NULL REFERENCES app_private.platby_category (pc_id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  pi_id_raw bigint REFERENCES app_private.platby_raw (pr_id)
    ON UPDATE RESTRICT
    ON DELETE RESTRICT,
  pi_amount numeric(10, 2) NOT NULL,
  pi_date date NOT NULL,
  pi_prefix int DEFAULT 2000 NOT NULL,
  id bigint GENERATED ALWAYS AS (pi_id) STORED NOT NULL UNIQUE,
  tenant_id bigint DEFAULT public.current_tenant_id() NOT NULL REFERENCES public.tenant (id)
    ON DELETE CASCADE,
  status public.payment_status DEFAULT CAST('paid' AS public.payment_status) NOT NULL
);

CREATE TABLE app_private.system_admin_user (
  user_id bigint NOT NULL PRIMARY KEY REFERENCES public.users (id)
    ON DELETE CASCADE,
  created_at timestamp with time zone DEFAULT now() NOT NULL,
  created_by bigint DEFAULT public.current_user_id() NOT NULL
);

CREATE TYPE crawler.fetch_status AS ENUM ('pending', 'ok', 'gone', 'error');

CREATE TYPE crawler.process_status AS ENUM ('pending', 'ok', 'error');

CREATE TABLE crawler.frontier (
  id bigint NOT NULL PRIMARY KEY,
  federation text NOT NULL,
  kind text NOT NULL,
  key text NOT NULL,
  discovered_at timestamp with time zone DEFAULT now() NOT NULL,
  last_fetched_at timestamp with time zone,
  fetch_status crawler.fetch_status DEFAULT CAST('pending' AS crawler.fetch_status) NOT NULL,
  process_status crawler.process_status DEFAULT CAST('pending' AS crawler.process_status) NOT NULL,
  error_count int DEFAULT 0 NOT NULL,
  next_fetch_at timestamp with time zone,
  meta jsonb DEFAULT '{}'::jsonb NOT NULL,
  UNIQUE (federation, kind, key)
);

CREATE TABLE crawler.html_response_cache (
  content_hash text GENERATED ALWAYS AS (encode(public.digest(content, 'sha256'::text), 'hex'::text)) STORED NOT NULL PRIMARY KEY,
  content text
);

CREATE TABLE crawler.html_response (
  id bigint NOT NULL PRIMARY KEY,
  frontier_id bigint NOT NULL REFERENCES crawler.frontier (id)
    ON DELETE CASCADE,
  url text NOT NULL,
  fetched_at timestamp with time zone DEFAULT now() NOT NULL,
  http_status int,
  error text,
  content_hash text REFERENCES crawler.html_response_cache (content_hash)
);

CREATE TABLE crawler.incremental_ranges (
  federation text NOT NULL,
  kind text NOT NULL,
  last_known int DEFAULT 0 NOT NULL,
  last_checked int DEFAULT 0 NOT NULL,
  PRIMARY KEY (federation, kind)
);

CREATE TABLE crawler.json_response_cache (
  content_hash text GENERATED ALWAYS AS (encode(public.digest(content::text, 'sha256'::text), 'hex'::text)) STORED NOT NULL PRIMARY KEY,
  content jsonb
);

CREATE TABLE crawler.json_response (
  id bigint NOT NULL PRIMARY KEY,
  frontier_id bigint NOT NULL REFERENCES crawler.frontier (id)
    ON DELETE CASCADE,
  url text NOT NULL,
  fetched_at timestamp with time zone DEFAULT now() NOT NULL,
  http_status int,
  error text,
  content_hash text REFERENCES crawler.json_response_cache (content_hash)
);

CREATE TABLE crawler.rate_limit_rule (
  host text NOT NULL PRIMARY KEY,
  max_requests int NOT NULL,
  per_interval interval NOT NULL,
  spacing interval GENERATED ALWAYS AS ((per_interval / CAST(max_requests AS double precision)) + '00:00:00.02'::interval) STORED NOT NULL,
  next_available_at timestamp with time zone DEFAULT CAST('1970-01-01 00:00:00+00' AS timestamp with time zone) NOT NULL,
  CHECK (max_requests > 0),
  CHECK (per_interval > '00:00:00'::interval)
);

CREATE TYPE federated.competitor_component_input AS (athlete_id bigint, role federated.competitor_role);

CREATE TYPE public.announcement_audience_type_input AS (id bigint, cohort_id bigint, audience_role public.announcement_audience_role);

CREATE TYPE public.announcement_type_input AS (id bigint, title text, body text, is_locked boolean, is_visible boolean, is_sticky boolean, scheduled_since timestamp with time zone, scheduled_until timestamp with time zone);

CREATE TYPE public.event_instance_trainer_type_input AS (id bigint, person_id bigint);

CREATE TYPE public.event_instance_type_input AS (id bigint, since timestamp with time zone, until timestamp with time zone, is_cancelled boolean, trainers public.event_instance_trainer_type_input[]);

CREATE TYPE public.event_overlaps_conflict AS (person_id bigint, person_name text, first_instance_id bigint, first_event_id bigint, first_event_name text, first_since timestamp with time zone, first_until timestamp with time zone, second_instance_id bigint, second_event_id bigint, second_event_name text, second_since timestamp with time zone, second_until timestamp with time zone, overlap_range tstzrange);

CREATE TYPE public.event_registration_type_input AS (id bigint, person_id bigint, couple_id bigint);

CREATE TYPE public.event_target_cohort_type_input AS (id bigint, cohort_id bigint);

CREATE TYPE public.event_trainer_type_input AS (id bigint, person_id bigint, lessons_offered int);

CREATE TYPE public.event_type_input AS (id bigint, name text, summary text, description text, description_member text, type public.event_type, location_id bigint, location_text text, capacity int, is_visible boolean, is_public boolean, is_locked boolean, enable_notes boolean, payment_type public.event_payment_type, member_price public.price, guest_price public.price);

CREATE TYPE public.jwt_token AS (exp int, user_id bigint, tenant_id bigint, username text, email text, my_person_ids pg_catalog.json, my_tenant_ids pg_catalog.json, my_cohort_ids pg_catalog.json, my_couple_ids pg_catalog.json, is_member boolean, is_trainer boolean, is_admin boolean, is_system_admin boolean);

CREATE TYPE public.register_to_event_type AS (event_id bigint, person_id bigint, couple_id bigint, note text, lessons public.event_lesson_demand[]);

CREATE TYPE public.scoreboard_record AS (person_id bigint, cohort_id bigint, lesson_total_score bigint, group_total_score bigint, event_total_score bigint, manual_total_score bigint, total_score bigint, ranking bigint);

CREATE TYPE public.trainer_group_attendance_completion AS (person_id int, total_instances int, filled_instances int, partially_filled_instances int, unfilled_instances int, filled_ratio double precision, total_attendances int, pending_attendances int);
