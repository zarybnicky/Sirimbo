import { CohortForm } from 'components/CohortForm';
import { CohortsList } from 'components/CohortList';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/skupiny" title="Nová skupina" />
      <CohortForm />
    </Item>
  );
};

Page.list = <CohortsList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];

export default Page;
