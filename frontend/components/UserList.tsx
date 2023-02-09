import { useMsmtExportQuery, useUserListQuery } from "lib/graphql/User";
import { Plus } from 'react-feather';
import { useRouter } from "next/router";
import { List } from "components/layout/List";
import { useRoleListQuery } from "lib/graphql/Roles";
import { useCohortListQuery } from "lib/graphql/Cohorts";
import { TextField } from "./TextField";
import React from "react";
import MiniSearch from 'minisearch';
import { UserFragment } from "lib/graphql/CurrentUser";
import { saveAs } from "file-saver";
import format from "date-fns/format";

export const UserList = () => {
  const router = useRouter();
  const { id } = router.query;
  const active = id ? id as string : null;

  const { data } = useUserListQuery();
  const { data: roles } = useRoleListQuery();
  const { data: cohorts } = useCohortListQuery();

  const exportMSMT = React.useCallback(async (e?: React.MouseEvent) => {
    e?.preventDefault();

    const { Workbook } = await import('exceljs');
    const data = await useMsmtExportQuery.fetcher()();
    const workbook = new Workbook();
    const worksheet = workbook.addWorksheet("MŠMT Export");

    worksheet.columns = [
      { header: 'JMENO', key: 'firstName' },
      { header: 'PRIJMENI', key: 'lastName' },
      { header: 'TITUL_PRED', key: 'prefixTitle' },
      { header: 'TITUL_ZA', key: 'suffixTitle' },
      { header: 'RODNE_CISLO', key: 'birthNumber' },
      { header: 'OBCANSTVI', key: 'nationality' },
      { header: 'DATUM_NAROZENI', key: 'birthDate' },
      { header: 'NAZEV_OBCE', key: 'city' },
      { header: 'NAZEV_CASTI_OBCE', key: 'district' },
      { header: 'NAZEV_ULICE', key: 'street' },
      { header: 'CISLO_POPISNE', key: 'conscriptionNumber' },
      { header: 'CISLO_ORIENTACNI', key: 'orientationNumber' },
      { header: 'PSC', key: 'postalCode' },
      { header: 'SPORTOVEC', key: 'isAthlete' },
      { header: 'SPORTOVCEM_OD', key: 'athleteSince' },
      { header: 'SPORTOVCEM_DO', key: 'athleteUntil' },
      { header: 'SPORTOVEC_CETNOST', key: 'athleteWeeklyActivity' },
      { header: 'SPORTOVEC_DRUH_SPORTU', key: 'sportType' },
      { header: 'SPORTOVEC_UCAST_SOUTEZE', key: 'isCompeting' },
      { header: 'TRENER', key: 'isTrainer' },
      { header: 'TRENEREM_OD', key: 'trainerSince' },
      { header: 'TRENEREM_DO', key: 'trainerUntil' },
      { header: 'TRENER_CETNOST', key: 'trainerWeeklyActivity' },
      { header: 'TRENER_DRUH_SPORTU', key: 'sportTypeTrainer' },
      { header: 'EXT_ID', key: 'extId' },
    ];

    worksheet.getRow(1).font = { bold: true };
    worksheet.columns.forEach(column => {
      column.width = (column?.header?.length || 0) + 10;
      column.alignment = { horizontal: 'center' };
    });

    data.members?.nodes.forEach(x => {
      worksheet.addRow({
        firstName: x.uJmeno,
        lastName: x.uPrijmeni,
        birthNumber: x.uRodneCislo,
        nationality: x.uNationality === '203' ? 'CZE' : x.uNationality,
        birthDate: x.uNarozeni ? format(new Date(x.uNarozeni), 'd.M.yyyy') : '',
        city: x.uCity,
        district: x.uDistrict,
        street: x.uStreet,
        conscriptionNumber: x.uConscriptionNumber,
        orientationNumber: x.uOrientationNumber,
        postalCode: x.uPostalCode?.replaceAll(' ', ''),
        isAthlete: x.uGroup === '3' ? '1' : '0',
        sportType: x.uGroup === '3' ? '66' : '',
        isCompeting: x.uGroup !== '3' ? '1' : '0',
        isTrainer: x.uGroup !== '3' ? '1' : '0',
        sportTypeTrainer: x.uGroup !== '3' ? '66' : '',
        athleteSince: x.oldestPayment ? format(new Date(x.oldestPayment), 'd.M.yyyy') : '',
        extId: x.id,
      });
    });

    const buf = await workbook.csv.writeBuffer();
    saveAs(new Blob([buf]), "Olymp-MSMT-2022.csv");
  }, [id]);

  const nodesById = React.useMemo(() => {
    return (data?.users?.nodes || []).reduce((byId, document) => {
      byId[document.id] = document;
      return byId;
    }, {} as { [key: string]: UserFragment });
  }, [data]);
  const [search, setSearch] = React.useState('');

  const index = React.useMemo(() => {
    const index = new MiniSearch({
      fields: ['id', 'uJmeno', 'uPrijmeni'],
      searchOptions: {
        fuzzy: 0.2,
        prefix: true,
      },
    });
    index.addAll(data?.users?.nodes || []);
    return index;
  }, [data]);

  const nodes = React.useMemo(() => {
    if (!search) {
      return data?.users?.nodes || [];
    }
    return index.search(search).map(({ id }) => nodesById[id]!);
  }, [index, search]);

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

        <List.TitleButton onClick={exportMSMT}>MŠMT Export</List.TitleButton>
      </div>

      <TextField
        type="search" className="w-full mt-2" placeholder="Vyhledat..."
        value={search} onChange={(e) => setSearch(e.currentTarget.value)}
      />
    </List.TitleBar>

    <List.Scroll>
      {nodes.map((item) => (
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
      ))}
    </List.Scroll>
  </List>;
}
