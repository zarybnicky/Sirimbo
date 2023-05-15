import { CohortGroupForm } from 'components/CohortGroupForm';
import { CohortGroupList } from 'components/CohortGroupList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/cohort-group" title="Nový tréninkový program" />
      <CohortGroupForm />
    </Item>
  );
}

Page.list = <CohortGroupList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peAktuality, PermissionLevel.P_OWNED];
Page.staticTitle = "Tréninkové programy";

export default Page;
