import { PageForm } from "components/PageForm";
import { usePageByIdQuery } from "lib/graphql/Page";
import { useRouter } from "next/router";
import { Layout } from 'components/layout/Layout';
import { Item } from 'components/layout/Item';
import { PageList } from 'components/PageList';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

export default function PageEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = usePageByIdQuery({ id: parseInt(id as string) }, { enabled: !!id, cacheTime: 0 });
  return <>
    <Item.Titlebar backHref="/admin/skupiny" title={data?.page?.title} />
    {data && <PageForm data={data.page || undefined} />}
  </>;
};

PageEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<PageList />} isDetail>{page}</Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.peSkupiny, PermissionLevel.P_OWNED,
);
