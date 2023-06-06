-- Write your migration here
CREATE INDEX ON "public"."room_attachment"("object_name");
CREATE INDEX ON "public"."tenant_attachment"("object_name");
CREATE INDEX ON "public"."skupiny"("cohort_group");
CREATE INDEX ON "public"."skupiny"("ordering");
CREATE INDEX ON "public"."cohort_group"("tenant");
CREATE INDEX ON "public"."room"("location");
