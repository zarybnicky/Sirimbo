import { Plus } from 'react-feather';
import { useRouter } from 'next/router';
import { List } from 'components/layout/List';
import { fromSlugArray } from 'lib/slugify';
import { useGqlQuery } from 'lib/query';
import { CohortGroupListDocument } from 'lib/graphql/CohortGroup';
import { FuzzyList } from './FuzzyList';

export function CohortGroupList() {
  const router = useRouter();
  const { data } = useGqlQuery(CohortGroupListDocument, {});
  const id = fromSlugArray(router.query.id);

  return (
    <List>
      <List.TitleBar title="Tréninkové programy">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/cohort-group/add"
        >
          Nový
        </List.TitleButton>
      </List.TitleBar>

      <FuzzyList
        data={data?.cohortGroups?.nodes || []}
        fields={['id', 'name']}
        renderItem={(n, item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{ pathname: '/admin/cohort-group/[id]', query: { id: item.id } }}
            title={item.name}
            className="pl-6"
            subtitle={!item.isPublic ? 'Skrytý' : null}
          />
        )}
      />
    </List>
  );
}
