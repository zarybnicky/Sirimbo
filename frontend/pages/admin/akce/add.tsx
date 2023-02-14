import { EventForm } from "components/EventForm";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { Item } from "components/layout/Item";

export default function EventAddPage() {
  return <Item>
    <Item.Titlebar backHref="/admin/akce" title="Nová akce" />
    <EventForm />
  </Item>;
};

export const getServerSideProps = withServerPermissions(
  PermissionKey.peAkce, PermissionLevel.P_OWNED,
);
