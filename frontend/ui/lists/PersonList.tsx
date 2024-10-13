import { TextField } from '@/ui/fields/text';
import React from 'react';
import { PersonListDocument } from '@/graphql/Person';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { ComboboxButton } from '@/ui/fields/Combobox';
import Link from 'next/link';
import { CreatePersonDialog } from '@/ui/CreatePersonDialog';
import { useAuth } from '@/ui/use-auth';
import { buttonCls } from '@/ui/style';
import { useSessionStorage } from "@/lib/use-local-storage";
import { cn } from '@/ui/cn';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';
import { useCohorts } from '@/ui/useCohorts';
import { StringParam, useQueryParam } from 'use-query-params';

const QueryParams = z.object({
  id: zRouterId,
});

export function PersonList() {
  const router = useTypedRouter(QueryParams);
  const id = router.query.id;
  const auth = useAuth();

  const [cohort, setCohort] = useSessionStorage('personfilter-cohort', undefined);
  const [isTrainer, setIsTrainer] = useSessionStorage('personfilter-trainer', undefined);
  const [isAdmin, setIsAdmin] = useSessionStorage('personfilter-admin', undefined);
  const [search, setSearch] = useSessionStorage('personfilter-search', '');

  const { data: cohorts } = useCohorts();
  const cohortOptions = React.useMemo(() => cohorts.map((x) => ({ id: x.id, label: x.name })), [cohorts]);
  const [tab] = useQueryParam('tab', StringParam);

  const [{ data }] = useQuery({
    query: PersonListDocument,
    variables: {
      inCohorts: cohort ? [cohort] : null,
      isAdmin: !!isAdmin || null,
      isTrainer: !!isTrainer || null,
    },
  });
  const nodes = React.useMemo(() => {
    return (data?.filteredPeopleList || []).map((item) => {
      const cohort = cohorts.find((x) => (item.cohortIds || []).includes(x.id));
      return {
        yearOfBirth: item.birthDate ? new Date(item.birthDate).getFullYear() : undefined,
        cohort: cohort?.name,
        cohortColor: cohort?.colorRgb,
        ...item,
      };
    });
  }, [data, cohorts]);

  const fuzzy = useFuzzySearch(
    nodes,
    ['id', 'name', 'cohort', "phone", "email", 'yearOfBirth'],
    search || '',
  );

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Členové</div>
        {auth.isAdmin && (
          <CreatePersonDialog />
        )}

        <div className="mt-2 w-full flex gap-2 justify-end">
          <ComboboxButton
            value={cohort}
            onChange={setCohort}
            placeholder="jen skupina"
            options={cohortOptions}
          />

          <button
            type="button"
            className={buttonCls({ size: 'sm', variant: isTrainer ? 'primary' : 'outline' })}
            onClick={() => setIsTrainer(x => x ? null : '1')}
          >
            Jen trenéři
          </button>
          <button
            type="button"
            className={buttonCls({ size: 'sm', variant: isAdmin ? 'primary' : 'outline' })}
            onClick={() => setIsAdmin(x => x ? null : '1')}
          >
            Jen správci
          </button>
        </div>

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Jméno, email, telefon"
          value={search || ''}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <Virtuoso
        className="grow h-full overflow-y-auto scrollbar"
        data={fuzzy}
        itemContent={(_n, item) => (
          <Link
            key={item.id}
            href={`/clenove/${item.id}${tab ? `?tab=${tab}` : ''}`}
            className={cn(
              'relative p-1.5 pl-5 mb-1 mr-1 rounded-lg grid',
              id === item.id
                ? 'font-semibold bg-primary text-white shadow-md'
                : 'bg-accent-2 border border-accent-5 hover:bg-accent-4',
            )}
          >
            <div>{item.name}</div>
            <div
              className={cn(
                'text-sm',
                id === item.id ? 'text-white' : 'text-neutral-11',
              )}
            >
              {[
                item.yearOfBirth,
                item.isAdmin ? 'Správce' : '',
                item.isTrainer ? 'Trenér' : '',
                item.isMember ? 'Člen' : '',
              ].concat(item.activeCouplesList?.flatMap(x => [x.man, x.woman].filter(x => x?.id !== item.id).map(x => `tančí s ${x?.name}`))).filter(Boolean).join(', ')}
            </div>
            <div
              className="absolute rounded-l-lg border border-neutral-6 w-4 shadow-sm inset-y-0 left-0"
              style={{
                backgroundColor: item.cohortColor || '#fff',
              }}
            />
          </Link>
        )}
      />
    </div>
  );
};
