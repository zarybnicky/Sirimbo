import { Plus } from 'lucide-react';
import { useRouter } from 'next/router';
import { List } from '@app/ui/List';
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
    <>
      <List.TitleBar title="Uživatelé">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/users/add"
        >
          Nový uživatel
        </List.TitleButton>

        <div className="mt-2 w-full flex gap-2 justify-end">
          <List.TitleButton
            active={router.asPath.endsWith('unconfirmed')}
            href="/admin/users/unconfirmed"
          >
            Nově registrovaní
          </List.TitleButton>

          <List.TitleButton onClick={doExportMSMT}>MŠMT Export</List.TitleButton>
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
      </List.TitleBar>

      <Virtuoso
        className="grow h-full overflow-y-auto scrollbar"
        data={fuzzy}
        itemContent={(_n, item) => (
          <List.Item
            key={item.id}
            className="pl-6"
            active={id === item.id}
            href={`/admin/users/${item.id}`}
            title={item.name}
            subtitle={item.yearOfBirth + ', ' + item.role}
          >
            <div
              className="absolute rounded-l-lg w-4 shadow-sm inset-y-0 left-0"
              style={{
                backgroundColor: item.cohortColor,
              }}
            />
          </List.Item>
        )}
      />
    </>
  );
};
