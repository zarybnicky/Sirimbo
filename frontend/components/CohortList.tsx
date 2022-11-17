import { useCohortListQuery } from "lib/graphql/Cohorts";
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";

export const CohortsList = () => {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useCohortListQuery();
  const active = id ? id as string : null;

  return <List>
    <List.TitleBar title="Skupiny">
      <List.TitleButton active={router.asPath.endsWith('add')} icon={Plus} href="/admin/skupiny/add">
        Nová skupina
      </List.TitleButton>
    </List.TitleBar>

    <List.Scroll>
      {data?.skupinies?.nodes?.map((item) => (
        <List.Item
          key={item.id}
          active={active === item.id} href={`/admin/skupiny/${item.id}`}
          title={item.sName}
          subtitle={[!item.sVisible ? "Skrytá" : null, item.sLocation].filter(Boolean).join(', ')}
        >
          <div className="absolute rounded-l-lg w-4 shadow-sm top-0 bottom-0 left-0" style={{ backgroundColor: item.sColorRgb }} />
        </List.Item>
      ))}
    </List.Scroll>
  </List>;
}
