import { NewCoupleForm } from '@app/ui/NewCoupleForm';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { CoupleList } from '@app/ui/entity-lists';
import { TitleBar } from '@app/ui/TitleBar';

const Page: NextPageWithLayout = () => {
  return (
    <div className="container py-4">
      <TitleBar backHref="/admin/pary" title="Nový pár" />
      <NewCoupleForm />
    </div>
  );
}

Page.list = <CoupleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];
Page.staticTitle = "Páry";

export default Page;
