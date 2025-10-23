
comment on function create_missing_cohort_subscription_payments is '@omit';
comment on function create_next_cohort_subscription_payment is '@omit';
comment on function get_current_user is '@omit';
comment on table dokumenty is E'@simpleCollections only
@omit create,update,delete';
comment on table announcement_audience is '@omit create,update,delete';

drop function if exists register_to_event;
alter table if exists platby_category set schema app_private;
alter table if exists platby_group_category set schema app_private;
alter table if exists platby_group set schema app_private;
alter table if exists platby_item set schema app_private;
alter table if exists platby_raw set schema app_private;
alter table if exists galerie_dir set schema app_private;
