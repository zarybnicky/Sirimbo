--! Previous: sha1:652bf2ab184977bf52de5c5eff53f759eaa87770
--! Hash: sha1:dc11ad358556cd0354cdd57b975907f739950a86

--! split: 1-current.sql
GRANT INSERT(
  event_id, first_name, last_name, prefix_title, suffix_title, nationality, birth_date, tax_identification_number, email, phone, note
) ON TABLE public.event_external_registration TO anonymous;

select app_private.drop_policies('public.event_external_registration');
CREATE POLICY admin_all ON event_external_registration TO administrator USING (true);
CREATE POLICY trainer_same_tenant ON event_external_registration TO trainer USING (
  app_private.can_trainer_edit_event(event_id)
) WITH CHECK (true);
CREATE POLICY admin_my ON event_external_registration TO member USING (
  (SELECT event_is_registration_open(event.*) FROM event WHERE event_id = event.id)
  AND (created_by = current_user_id())
);
CREATE POLICY view_visible_event ON public.event_external_registration FOR SELECT TO member USING (
  EXISTS (SELECT 1 FROM event WHERE event_id = event.id)
);
CREATE POLICY register_public ON public.event_external_registration FOR INSERT TO anonymous WITH CHECK (
  (SELECT is_public FROM event WHERE event_id = event.id)
);
