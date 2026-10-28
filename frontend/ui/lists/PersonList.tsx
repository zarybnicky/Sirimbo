import { ListItem } from '@/ui/ListItem';
import { UserRound } from 'lucide-react';
import { TextField } from '@/ui/fields/text';
import React from 'react';
import { PersonFacetsDocument, PersonListDocument } from '@/graphql/Person';
import { useFuzzySearch } from '@/lib/useFuzzySearch';
import { useClient, useQuery } from 'urql';
import { ComboboxButton } from '@/ui/fields/Combobox';
import { FacetFilter } from '@/ui/FacetFilter';
import { CreatePersonDialog } from '@/ui/CreatePersonDialog';
import { useAuth } from '@/lib/auth';
import { buttonCls } from '@/ui/style';
import { useSessionStorage } from '@/lib/useSessionStorage';
import { compareText } from '@/lib/compare';
import { useParams } from 'next/navigation';
import { useQueryState } from 'nuqs';
import { exportNsa } from '../reports/export-nsa';
import { CohortListDocument } from '@/graphql/Cohorts';

export function PersonList() {
  const id = useParams<{ id?: string }>()?.id;
  const auth = useAuth();
  const client = useClient();

  const [cohort, setCohort] = useSessionStorage('personfilter-cohort');
  const [isTrainer, setIsTrainer] = useSessionStorage('personfilter-trainer');
  const [isAdmin, setIsAdmin] = useSessionStorage('personfilter-admin');
  const [includeFormer, setIncludeFormer] = useSessionStorage(
    'personfilter-include-former',
  );
  const [search, setSearch] = useSessionStorage('personfilter-search', '');
  const [{ data: cohorts }] = useQuery({ query: CohortListDocument });

  const [tab] = useQueryState('tab');
  const membership = {
    inCohorts: cohort === 'none' ? [] : cohort ? [cohort] : null,
    isAdmin: !!isAdmin || null,
    isTrainer: !!isTrainer || null,
    state: includeFormer ? 'FORMER' as const : 'CURRENT' as const,
  };
  const [{ data }] = useQuery({
    query: PersonListDocument,
    variables: { condition: { membership } },
  });
  const [facetResult] = useQuery({
    query: PersonFacetsDocument,
    variables: {
      inCohorts: membership.inCohorts,
      isAdmin: membership.isAdmin,
      isTrainer: membership.isTrainer,
      membershipStates: [membership.state.toLowerCase()],
    },
  });
  const [facetData, setFacetData] = React.useState(facetResult.data);
  if (facetResult.data !== undefined && facetResult.data !== facetData) {
    setFacetData(facetResult.data);
  }
  const counts: Record<string, Record<string, number>> | undefined = facetData
    ? {}
    : undefined;
  for (const { facet, value, count } of facetData?.personFacetsList ?? []) {
    (counts![facet!] ??= {})[value!] = count ?? 0;
  }
  const nodes = React.useMemo(() => {
    return (data?.people?.nodes || [])
      .toSorted((a, b) =>
        compareText(a.lastName, b.lastName) || compareText(a.firstName, b.firstName),
      )
      .map((item) => {
        const activeCohorts = item.cohortMembershipsList.flatMap((x) =>
          x.status === 'ACTIVE' && x.cohort ? [x.cohort] : [],
        );
        return {
          yearOfBirth: item.birthDate
            ? new Date(item.birthDate).getFullYear()
            : undefined,
          cohortNames: activeCohorts.map((x) => x.name).join(', '),
          cohortColors: activeCohorts.map((x) => x.colorRgb),
          ...item,
        };
      });
  }, [data]);

  const fuzzy = useFuzzySearch(
    nodes,
    ['id', 'name', 'cohortNames', 'phone', 'email', 'yearOfBirth'],
    search || '',
  );
  const filters = [
    ...(includeFormer ? ['former'] : []),
    ...(isTrainer ? ['trainer'] : []),
    ...(isAdmin ? ['admin'] : []),
  ];

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Členové</div>
        <div className="mt-2 flex gap-2 justify-end">
          {auth.isAdmin && (
            <button
              type="button"
              className={buttonCls({ size: 'xs' })}
              onClick={() => exportNsa(client)}
            >
              NSA export
            </button>
          )}
          {auth.isAdmin && <CreatePersonDialog />}
        </div>

        <div className="mt-2 w-full flex flex-wrap gap-2 justify-end">
          <ComboboxButton
            value={cohort}
            onChange={setCohort}
            placeholder="skupina"
            options={[
              {
                id: 'none',
                label: 'Bez skupiny',
                count: counts ? (counts.cohort?.none ?? 0) : undefined,
              },
              ...(cohorts?.cohortsList ?? []).map((x) => ({
                id: x.id,
                label: x.name,
                count: counts ? (counts.cohort?.[x.id] ?? 0) : undefined,
              })),
            ]}
          />

          <FacetFilter
            inline
            label=""
            value={filters}
            options={[
              {
                value: 'former',
                label: 'Bývalí',
                count: counts ? (counts.state?.former ?? 0) : undefined,
              },
              {
                value: 'trainer',
                label: 'Trenéři',
                count: counts ? (counts.trainer?.true ?? 0) : undefined,
              },
              {
                value: 'admin',
                label: 'Správci',
                count: counts ? (counts.admin?.true ?? 0) : undefined,
              },
            ]}
            onChange={(values) => {
              setIncludeFormer(values.includes('former') ? '1' : null);
              setIsTrainer(values.includes('trainer') ? '1' : null);
              setIsAdmin(values.includes('admin') ? '1' : null);
            }}
          />
        </div>

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Jméno, email, telefon"
          value={search || ''}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <div className="grow h-full overflow-y-auto scrollbar">
        {fuzzy.map((item) => (
          <ListItem
            key={item.id}
            href={tab ? `/clenove/${item.id}?tab=${tab}` : `/clenove/${item.id}`}
            title={item.name}
            subtitle={[
              item.yearOfBirth,
              item.isAdmin ? 'Správce' : '',
              item.isTrainer ? 'Trenér' : '',
              item.isMember ? 'Člen' : '',
              ...(item.couplesList ?? []).flatMap((x) =>
                [x.man, x.woman]
                  .filter((x) => x?.id !== item.id)
                  .map((x) => `tančí s ${x?.name}`),
              ),
            ].filter(Boolean).join(', ')}
            selected={id === item.id}
            imageUrl={item.image?.url}
            icon={<UserRound className="size-5" />}
            colors={item.cohortColors.length > 0 ? item.cohortColors : ['#fff']}
          />
        ))}
      </div>
    </div>
  );
}
