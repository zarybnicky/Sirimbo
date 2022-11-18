import { usePageListQuery } from 'lib/graphql/Page';
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";

export const PageList = () => {
  const router = useRouter();
  const { id } = router.query;
  const { data } = usePageListQuery();
  const active = parseInt(id ? id as string : '0', 10);

  return <List>
    <List.TitleBar title="Stránky">
      <List.TitleButton active={router.asPath.endsWith('add')} icon={Plus} href="/admin/page/add">
        Nová stránka
      </List.TitleButton>
    </List.TitleBar>

    <List.Scroll>
      {data?.pages?.nodes?.map((item) => (
        <List.Item
          key={item.id}
          active={active === item.id} href={`/admin/page/${item.id}`}
          title={item.title}
          subtitle={item.url}
        >
        </List.Item>
      ))}
    </List.Scroll>
  </List>;
}
