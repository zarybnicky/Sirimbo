CREATE VIEW public.akce_item AS
 SELECT attendee_user.id AS ai_id,
    attendee_user.event_id AS ai_id_rodic,
    attendee_user.user_id AS ai_user,
    attendee_user.birth_year AS ai_rok_narozeni,
    attendee_user.notes
   FROM public.attendee_user;

COMMENT ON VIEW public.akce_item IS '@foreignKey (ai_id_rodic) references akce (a_id)
@foreignKey (ai_user) references users (u_id)';

GRANT ALL ON TABLE public.akce_item TO anonymous;


