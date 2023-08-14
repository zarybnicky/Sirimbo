import { useRouter } from 'next/router';
import { CohortListDocument } from '@app/graphql/Cohorts';
import { TextField } from '@app/ui/fields/text';
import React from 'react';
import { exportMSMT } from '@app/ui/export-msmt';
import { fromSlugArray } from '@app/ui/slugify';
import { PersonListDocument } from '@app/graphql/Person';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { Combobox } from './Combobox';
import Link from 'next/link';
import classNames from 'classnames';
import { buttonCls } from './style/button';
import { useAuth } from './use-auth';

export function UserList() {
  const router = useRouter();
  const { tenants } = useAuth();

  const [cohort, setCohort] = React.useState<string | null>(null);
  const [{ data: cohorts }] = useQuery({ query: CohortListDocument });
  const cohortOptions = React.useMemo(() => {
    return (cohorts?.skupinies?.nodes || []).map((x) => ({ id: x.id, label: x.sName }));
  }, [cohorts]);

  const [isTrainer, setIsTrainer] = React.useState<boolean | null>(null);
  const [isAdmin, setIsAdmin] = React.useState<boolean | null>(null);

  const [{ data }] = useQuery({
    query: PersonListDocument,
    variables: { inTenants: tenants.map(x => x.id), inCohort: cohort || null, isAdmin: isAdmin || null, isTrainer: isTrainer || null },
  });
  const id = fromSlugArray(router.query.id);

  const nodes = React.useMemo(() => {
    return (data?.filteredPeopleList || []).map((item) => ({
      id: item.id,
      name: `${item.firstName} ${item.lastName}`,
      yearOfBirth: new Date(item.birthDate).getFullYear(),
      cohort:  cohorts?.skupinies?.nodes.find((x) => x.id === item.uSkupina)?.sName,
      cohortColor: cohorts?.skupinies?.nodes.find((x) => x.id === item.uSkupina)?.sColorRgb,
    }));
  }, [data, cohorts?.skupinies?.nodes]);

  const doExportMSMT = React.useCallback((e?: React.MouseEvent) => {
    e?.preventDefault();
    exportMSMT();
  }, []);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(
    nodes,
    ['id', 'name', 'cohort', 'yearOfBirth'],
    search,
  );

  // TODO: Sign in as
  // TODO: Duplicate people

  return (
    <>
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Uživatelé</div>
        {/* <a
          href="/users/add"
          className={buttonCls({
            size: 'sm',
            variant: router.asPath.endsWith('add') ? 'primary' : 'outline',
          })}
        >
          <Plus />
          Nový uživatel
        </a> */}

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

          <button
            className={buttonCls({ size: 'sm', variant: 'outline' })}
            onClick={doExportMSMT}
          >
            MŠMT Export
          </button>
        </div>

        <Combobox
          value={cohort}
          onChange={setCohort}
          placeholder="tréninková skupina"
          options={cohortOptions}
        />
        <Combobox
          value={role}
          onChange={setRole}
          placeholder="uživatelská role"
          options={roleOptions}
        />

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <Virtuoso
        className="grow h-full overflow-y-auto scrollbar"
        data={fuzzy}
        itemContent={(_n, item) => (
          <Link
            key={item.id}
            href={`/users/${item.id}`}
            className={classNames(
              'relative p-2 pl-5 mr-2 my-1 rounded-lg grid',
              id === item.id
                ? 'font-semibold bg-primary text-white shadow-md'
                : 'hover:bg-neutral-4',
            )}
          >
            <div>{item.name}</div>
            <div
              className={classNames(
                'text-sm',
                id === item.id ? 'text-white' : 'text-neutral-11',
              )}
            >
              {item.yearOfBirth}, {item.role}
            </div>
            <div
              className="absolute rounded-l-lg w-4 shadow-sm inset-y-0 left-0"
              style={{
                backgroundColor: item.cohortColor,
              }}
            />
          </Link>
        )}
      />
    </>
  );
};
