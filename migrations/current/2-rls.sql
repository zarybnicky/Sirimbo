ALTER TABLE public.person_invitation
  DROP CONSTRAINT person_invitation_person_id_fkey;
ALTER TABLE public.person_invitation
  ADD CONSTRAINT person_invitation_person_id_fkey
  FOREIGN KEY (person_id)
  REFERENCES public.person (id)
  ON DELETE cascade;

ALTER TABLE public.person_invitation
  DROP CONSTRAINT person_invitation_tenant_id_fkey;
ALTER TABLE public.person_invitation
  ADD CONSTRAINT person_invitation_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE cascade;

ALTER TABLE app_private.crm_activity
  DROP CONSTRAINT crm_activity_prospect_fkey;
ALTER TABLE app_private.crm_activity
  ADD CONSTRAINT crm_activity_prospect_fkey
  FOREIGN KEY (prospect)
  REFERENCES app_private.crm_prospect (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.aktuality
  DROP CONSTRAINT aktuality_tenant_id_fkey;
ALTER TABLE public.aktuality
  ADD CONSTRAINT aktuality_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.attachment
  DROP CONSTRAINT attachment_uploaded_by_fkey;
ALTER TABLE public.attachment
  ADD CONSTRAINT attachment_uploaded_by_fkey
  FOREIGN KEY (uploaded_by)
  REFERENCES public.users (u_id)
  ON DELETE set null;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.cohort_group
  DROP CONSTRAINT cohort_group_tenant_id_fkey;
ALTER TABLE public.cohort_group
  ADD CONSTRAINT cohort_group_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.cohort_membership
  DROP CONSTRAINT cohort_membership_tenant_id_fkey;
ALTER TABLE public.cohort_membership
  ADD CONSTRAINT cohort_membership_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.dokumenty
  DROP CONSTRAINT dokumenty_tenant_id_fkey;
ALTER TABLE public.dokumenty
  ADD CONSTRAINT dokumenty_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.event
  DROP CONSTRAINT event_tenant_id_fkey;
ALTER TABLE public.event
  ADD CONSTRAINT event_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.form_responses
  DROP CONSTRAINT form_responses_tenant_id_fkey;
ALTER TABLE public.form_responses
  ADD CONSTRAINT form_responses_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.galerie_dir
  DROP CONSTRAINT galerie_dir_tenant_id_fkey;
ALTER TABLE public.galerie_dir
  ADD CONSTRAINT galerie_dir_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.galerie_foto
  DROP CONSTRAINT galerie_foto_tenant_id_fkey;
ALTER TABLE public.galerie_foto
  ADD CONSTRAINT galerie_foto_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.location_attachment
  DROP CONSTRAINT location_attachment_location_id_fkey;
ALTER TABLE public.location_attachment
  ADD CONSTRAINT location_attachment_location_id_fkey
  FOREIGN KEY (location_id)
  REFERENCES public.location (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.location_attachment
  DROP CONSTRAINT location_attachment_object_name_fkey;
ALTER TABLE public.location_attachment
  ADD CONSTRAINT location_attachment_object_name_fkey
  FOREIGN KEY (object_name)
  REFERENCES public.attachment (object_name)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.otp_token
  DROP CONSTRAINT otp_token_tenant_id_fkey;
ALTER TABLE public.otp_token
  ADD CONSTRAINT otp_token_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.otp_token
  DROP CONSTRAINT otp_token_user_id_fkey;
ALTER TABLE public.otp_token
  ADD CONSTRAINT otp_token_user_id_fkey
  FOREIGN KEY (user_id)
  REFERENCES public.users (u_id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_category_group
  DROP CONSTRAINT platby_category_group_tenant_id_fkey;
ALTER TABLE public.platby_category_group
  ADD CONSTRAINT platby_category_group_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_category
  DROP CONSTRAINT platby_category_tenant_id_fkey;
ALTER TABLE public.platby_category
  ADD CONSTRAINT platby_category_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_group_skupina
  DROP CONSTRAINT platby_group_skupina_tenant_id_fkey;
ALTER TABLE public.platby_group_skupina
  ADD CONSTRAINT platby_group_skupina_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_group
  DROP CONSTRAINT platby_group_tenant_id_fkey;
ALTER TABLE public.platby_group
  ADD CONSTRAINT platby_group_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_item
  DROP CONSTRAINT platby_item_tenant_id_fkey;
ALTER TABLE public.platby_item
  ADD CONSTRAINT platby_item_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.platby_raw
  DROP CONSTRAINT platby_raw_tenant_id_fkey;
ALTER TABLE public.platby_raw
  ADD CONSTRAINT platby_raw_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.room_attachment
  DROP CONSTRAINT room_attachment_object_name_fkey;
ALTER TABLE public.room_attachment
  ADD CONSTRAINT room_attachment_object_name_fkey
  FOREIGN KEY (object_name)
  REFERENCES public.attachment (object_name)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.room_attachment
  DROP CONSTRAINT room_attachment_room_id_fkey;
ALTER TABLE public.room_attachment
  ADD CONSTRAINT room_attachment_room_id_fkey
  FOREIGN KEY (room_id)
  REFERENCES public.room (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.room
  DROP CONSTRAINT room_location_fkey;
ALTER TABLE public.room
  ADD CONSTRAINT room_location_fkey
  FOREIGN KEY (location)
  REFERENCES public.location (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.skupiny
  DROP CONSTRAINT skupiny_cohort_group_fkey;
ALTER TABLE public.skupiny
  ADD CONSTRAINT skupiny_cohort_group_fkey
  FOREIGN KEY (cohort_group)
  REFERENCES public.cohort_group (id)
  ON DELETE set null;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.skupiny
  DROP CONSTRAINT skupiny_tenant_id_fkey;
ALTER TABLE public.skupiny
  ADD CONSTRAINT skupiny_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.tenant_attachment
  DROP CONSTRAINT tenant_attachment_object_name_fkey;
ALTER TABLE public.tenant_attachment
  ADD CONSTRAINT tenant_attachment_object_name_fkey
  FOREIGN KEY (object_name)
  REFERENCES public.attachment (object_name)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.tenant_attachment
  DROP CONSTRAINT tenant_attachment_tenant_id_fkey;
ALTER TABLE public.tenant_attachment
  ADD CONSTRAINT tenant_attachment_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


-- fk-on-delete: All foreign key constraints should have an ON DELETE action.
-- Drop the constraint, and replace it with a constraint with `ON DELETE` clause.
ALTER TABLE public.tenant_location
  DROP CONSTRAINT tenant_location_location_id_fkey;
ALTER TABLE public.tenant_location
  ADD CONSTRAINT tenant_location_location_id_fkey
  FOREIGN KEY (location_id)
  REFERENCES public.location (id)
  ON DELETE CASCADE;


ALTER TABLE public.tenant_location
  DROP CONSTRAINT tenant_location_tenant_id_fkey;
ALTER TABLE public.tenant_location
  ADD CONSTRAINT tenant_location_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


ALTER TABLE public.upozorneni_skupiny
  DROP CONSTRAINT upozorneni_skupiny_tenant_id_fkey;
ALTER TABLE public.upozorneni_skupiny
  ADD CONSTRAINT upozorneni_skupiny_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


ALTER TABLE public.upozorneni
  DROP CONSTRAINT upozorneni_tenant_id_fkey;
ALTER TABLE public.upozorneni
  ADD CONSTRAINT upozorneni_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;


ALTER TABLE public.users
  DROP CONSTRAINT users_tenant_id_fkey;
ALTER TABLE public.users
  ADD CONSTRAINT users_tenant_id_fkey
  FOREIGN KEY (tenant_id)
  REFERENCES public.tenant (id)
  ON DELETE CASCADE;
