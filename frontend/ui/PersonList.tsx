import { TextField } from '@/ui/fields/text';
import React from 'react';
import { PersonListDocument } from '@/graphql/Person';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { ComboboxButton } from './Combobox';
import Link from 'next/link';
import { CreatePersonDialog } from './CreatePersonDialog';
import { useAuth } from './use-auth';
import { buttonCls } from './style';
import { useLocalStorage } from "@/lib/use-local-storage";
import { cn } from './cn';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { z } from 'zod';
import { useCohorts } from './useCohorts';

const QueryParams = z.object({
  id: zRouterId,
});

export function PersonList() {
  const router = useTypedRouter(QueryParams);
  const id = router.query.id;
  const { perms } = useAuth();

  const [cohort, setCohort] = useLocalStorage('personfilter-cohort', undefined);
  const [isTrainer, setIsTrainer] = useLocalStorage('personfilter-trainer', undefined);
  const [isAdmin, setIsAdmin] = useLocalStorage('personfilter-admin', undefined);
  const [search, setSearch] = useLocalStorage('personfilter-search', '');

  const { data: cohorts } = useCohorts();
  const cohortOptions = React.useMemo(() => cohorts.map((x) => ({ id: x.id, label: x.sName })), [cohorts]);

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
        cohort: cohort?.sName,
        cohortColor: cohort?.sColorRgb,
        ...item,
      };
    });
  }, [data, cohorts]);

  const fuzzy = useFuzzySearch(
    nodes,
    ['id', 'name', 'cohort', "phone", "email", 'yearOfBirth'],
    search || '',
  );

  // TODO: Sign in as
  // TODO: Duplicate people

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Členové</div>
        {perms.isAdmin && (
          <CreatePersonDialog />
        )}

        <div className="mt-2 w-full flex gap-2 justify-end">
          {/* <a
            href="/users/unconfirmed"
            className={buttonCls({
              size: 'sm',
              variant: router.asPath.endsWith('unconfirmed') ? 'primary' : 'outline',
            })}
          >
            Nově registrovaní
          </a> */}

          {/* <button
            className={buttonCls({ size: 'sm', variant: 'outline' })}
            onClick={doExportMSMT}
          >
            MŠMT Export
          </button> */}

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
            href={`/clenove/${item.id}`}
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
            > {[
              item.yearOfBirth,
              item.isAdmin ? 'Správce' : '',
              item.isTrainer ? 'Trenér' : '',
              item.isMember ? 'Člen' : '',
              (() => {
                const couples = item.activeCouplesList?.flatMap(x => [x.man, x.woman].filter(x => x?.id != item.id).map(x => x?.name)).join(', ');
                return couples ? `tančí s ${couples}` : undefined;
              })(),
            ].filter(Boolean).join(', ')}
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
