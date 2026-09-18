select app_private.drop_policies('public.document');

create policy current_tenant on document as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on document to administrator using (true);
create policy trainer_all on document to trainer using (true);
create policy member_view on document for select to member using (show_to_members);

select app_private.drop_policies('public.document_node');

create policy current_tenant on document_node as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_all on document_node to administrator using (true);
create policy trainer_all on document_node to trainer using (true);
create policy member_view on document_node for select to member
  using (document_id in (select id from document));

select app_private.drop_policies('public.document_node_tag');

-- Tags are derived from node content by a trigger, so nobody writes them directly.
create policy current_tenant on document_node_tag as restrictive
  using (tenant_id = (select current_tenant_id()));
create policy admin_view on document_node_tag for select to administrator using (true);
create policy trainer_view on document_node_tag for select to trainer using (true);
create policy member_view on document_node_tag for select to member
  using (node_id in (select id from document_node));

grant all on table document, document_node to anonymous;
grant select on table document_node_tag to anonymous;
