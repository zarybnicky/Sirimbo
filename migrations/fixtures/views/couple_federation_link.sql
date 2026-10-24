create or replace view public.couple_federation_link
with (security_invoker = true) as
with links as materialized (
  select * from public.person_federation_link
)
select c.id as couple_id, fc.id as competitor_id
from public.couple c
join links ml on ml.person_id = c.man_id
join federated.person mp on (mp.federation, mp.external_id) = (ml.federation, ml.external_id)
join federated.competitor_component mc on mc.person_id = mp.id and mc.role = 'lead'
join federated.competitor fc on fc.id = mc.competitor_id and fc.competitor_type = 'couple'
join federated.competitor_component wc on wc.competitor_id = fc.id and wc.role = 'follow'
join federated.person wp on wp.id = wc.person_id
join links wl on wl.person_id = c.woman_id
  and (wl.federation, wl.external_id) = (wp.federation, wp.external_id);

comment on view public.couple_federation_link is '@primaryKey couple_id,competitor_id
@foreignKey (couple_id) references public.couple (id)
@foreignKey (competitor_id) references federated.competitor (id)
@behavior -single -list -connection -insert -update -delete';
grant select on public.couple_federation_link to anonymous;
