select app_private.drop_policies('public.document');

create policy current_tenant on document as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on document to administrator using (true);
create policy trainer_all on document to trainer using (true);
-- The document is the tenant's container, not an audience: who may read what is
-- carried by each node's visibility.
create policy member_view on document for select to member using (true);
create policy public_view on document for select to anonymous using (true);

select app_private.drop_policies('public.document_node');

create policy current_tenant on document_node as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on document_node to administrator using (true);
create policy trainer_all on document_node to trainer using (true);
create policy member_view on document_node for select to member
  using (visibility in ('members', 'public'));
create policy public_view on document_node for select to anonymous
  using (visibility = 'public');

select app_private.drop_policies('public.document_node_tag');

-- Tags are derived from node content by a trigger, so nobody writes them directly.
create policy current_tenant on document_node_tag as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_view on document_node_tag for select to administrator using (true);
create policy trainer_view on document_node_tag for select to trainer using (true);
-- A tag is as visible as the node it was projected from.
create policy node_view on document_node_tag as restrictive for select
  using (exists (select 1 from document_node node where node.id = node_id));

select app_private.drop_policies('public.document_node_file');

-- Also derived from content, and as visible as the node it came from.
create policy current_tenant on document_node_file as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy node_view on document_node_file for select
  using (exists (select 1 from document_node node where node.id = node_id));

grant all on table document, document_node to anonymous;
grant select on table document_node_tag, document_node_file to anonymous;
