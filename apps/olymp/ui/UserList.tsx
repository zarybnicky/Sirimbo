import { Plus } from 'lucide-react';
import { useRouter } from 'next/router';
import { RoleListDocument } from '@app/graphql/Roles';
import { CohortListDocument } from '@app/graphql/Cohorts';
import { TextField } from '@app/ui/fields/text';
import React from 'react';
import { exportMSMT } from '@app/ui/export-msmt';
import { fromSlugArray } from '@app/ui/slugify';
import { UserListDocument } from '@app/graphql/User';
import { useFuzzySearch } from '@app/ui/use-fuzzy-search';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { Combobox } from './Combobox';
import { cn } from './cn';
import Link from 'next/link';
import classNames from 'classnames';

export const UserList = () => {
  const router = useRouter();

  const [cohort, setCohort] = React.useState<string | null>(null);
  const [{ data: cohorts }] = useQuery({query: CohortListDocument});
  const cohortOptions = React.useMemo(() => {
    return (cohorts?.skupinies?.nodes || []).map(x => ({ id: x.id, label: x.sName }));
  }, [cohorts])

  const [role, setRole] = React.useState<string | null>(null);
  const [{ data: roles }] = useQuery({query: RoleListDocument});
  const roleOptions = React.useMemo(() => {
    return (roles?.permissions?.nodes || []).map(x => ({ id: x.id, label: x.peName }));
  }, [roles])

  const [{ data }] = useQuery({query: UserListDocument, variables: { cohort: cohort || undefined, role: role || undefined }});
  const id = fromSlugArray(router.query.id);

  const nodes = React.useMemo(() => {
    return (data?.users?.nodes || []).map((item) => ({
      id: item.id,
      name: `${item.uJmeno} ${item.uPrijmeni}`,
      role: roles?.permissions?.nodes.find((x) => x.id === item.uGroup)?.peName,
      cohort: cohorts?.skupinies?.nodes.find((x) => x.id === item.uSkupina)?.sName,
      yearOfBirth: new Date(item.uNarozeni).getFullYear(),
      cohortColor: cohorts?.skupinies?.nodes.find((x) => x.id === item.uSkupina)
        ?.sColorRgb,
    }));
  }, [data, roles?.permissions?.nodes, cohorts?.skupinies?.nodes]);

  const doExportMSMT = React.useCallback((e?: React.MouseEvent) => {
    e?.preventDefault();
    exportMSMT();
  }, []);

  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(
    nodes,
    ['id', 'name', 'role', 'cohort', 'yearOfBirth'],
    search,
  );

  // TODO: Sign in as
  // TODO: Duplicate people

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Uživatelé</div>
        <a href="/admin/users/add" className={cn('button-nav', router.asPath.endsWith('add') ? 'active' : '')}>
          <Plus />
          Nový uživatel
        </a>

        <div className="mt-2 w-full flex gap-2 justify-end">
          <a href="/admin/users/unconfirmed" className={cn('button-nav', router.asPath.endsWith('add') ? 'active' : '')}>
            Nově registrovaní
          </a>

          <button className="button-nav" onClick={doExportMSMT}>
            MŠMT Export
          </button>
        </div>

        <Combobox value={cohort} onChange={setCohort} placeholder="tréninková skupina" options={cohortOptions} />
        <Combobox value={role} onChange={setRole} placeholder="uživatelská role" options={roleOptions} />

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
            href={`/admin/users/${item.id}`}
            className={classNames(
              'relative p-2 pl-5 mr-2 my-1 rounded-lg grid',
              id === item.id ? 'font-semibold bg-primary text-white shadow-md' : 'hover:bg-neutral-4',
            )}
          >
            <div>{item.name}</div>
            <div className={classNames('text-sm', id === item.id ? 'text-white' : 'text-neutral-11')}>
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
    </div>
  );
};
