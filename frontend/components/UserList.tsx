import { useUserListQuery } from "lib/graphql/User";
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";
import { useRoleListQuery } from "lib/graphql/Roles";
import { useCohortListQuery } from "lib/graphql/Cohorts";
import { TextField } from "./TextField";
import React from "react";
import { FuzzyList } from "./FuzzyList";
import { exportMSMT } from "lib/export-msmt";

export const UserList = () => {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? id as string : null;

  const { data } = useUserListQuery();
  const { data: roles } = useRoleListQuery();
  const { data: cohorts } = useCohortListQuery();

  const doExportMSMT = React.useCallback((e?: React.MouseEvent) => {
    e?.preventDefault();
    exportMSMT();
  }, [id]);
  const [search, setSearch] = React.useState('');

  // Sign in as

  return <List>
    <List.TitleBar title="Uživatelé">
      <List.TitleButton active={router.asPath.endsWith('add')} icon={Plus} href="/admin/users/add">
        Nový uživatel
      </List.TitleButton>

      <div className="mt-2 w-full flex gap-2 justify-end">
        <List.TitleButton active={router.asPath.endsWith('unconfirmed')} href="/admin/users/unconfirmed">
          Nově registrovaní
        </List.TitleButton>

        <List.TitleButton onClick={doExportMSMT}>MŠMT Export</List.TitleButton>
      </div>

      <TextField
        type="search" className="w-full mt-2" placeholder="Vyhledat..."
        value={search} onChange={(e) => setSearch(e.currentTarget.value)}
      />
    </List.TitleBar>

    <List.Scroll>
      <FuzzyList
        data={data?.users?.nodes || []}
        fields={['id', 'uJmeno', 'uPrijmeni']}
        search={search}
        renderItem={(item) => (
          <List.Item
            key={item.id}
            active={active === item.id} href={`/admin/users/${item.id}`}
            title={`${item.uJmeno} ${item.uPrijmeni}`}
            subtitle={new Date(item.uNarozeni).getFullYear() + ', ' + roles?.permissions?.nodes.find(x => x.id === item.uGroup)?.peName}
          >
            <div className="absolute rounded-l-lg w-4 shadow-sm top-0 bottom-0 left-0" style={{
              backgroundColor: cohorts?.skupinies?.nodes.find(x => x.id === item.uSkupina)?.sColorRgb,
            }} />
          </List.Item>
        )}
      />
    </List.Scroll>
  </List>;
}
