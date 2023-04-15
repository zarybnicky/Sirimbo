import { ReservationForm } from 'components/ReservationForm';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { ReservationList } from 'components/ReservationList';
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';

export default function ReservationAddPage() {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/nabidka" title="Nová nabídka" />
      <ReservationForm />
    </Item>
  );
}

ReservationAddPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<ReservationList />} isDetail>
    {page}
  </Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNabidka,
  PermissionLevel.P_OWNED,
);
