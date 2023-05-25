import { CohortForm } from 'components/CohortForm';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import type { NextPageWithLayout } from 'pages/_app';
import { CohortList } from 'lib/entity-lists';

const Page: NextPageWithLayout = () => {
  return (
    <Item>
      <Item.Titlebar backHref="/admin/skupiny" title="Nová skupina" />
      <CohortForm />
    </Item>
  );
};

Page.list = <CohortList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.peSkupiny, PermissionLevel.P_OWNED];
Page.staticTitle = "Skupiny";

export default Page;
