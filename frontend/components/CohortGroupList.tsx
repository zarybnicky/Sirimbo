import { useCohortGroupListQuery } from "lib/graphql/CohortGroup";
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";

export function CohortGroupList() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useCohortGroupListQuery();
  const active = id ? id as string : null;

  return <List>
    <List.TitleBar title="Tréninkové programy">
      <List.TitleButton active={router.asPath.endsWith('add')} icon={Plus} href="/admin/cohort-group/add">
        Nový
      </List.TitleButton>
    </List.TitleBar>

    <List.Scroll>
      {data?.cohortGroups?.nodes?.map((item) => (
        <List.Item
          key={item.id}
          active={active === item.id}
          href={`/admin/cohort-group/${item.id}`}
          title={item.name}
          className="pl-6"
          subtitle={!item.isPublic ? "Skrytý" : null}
        />
      ))}
    </List.Scroll>
  </List>;
}
