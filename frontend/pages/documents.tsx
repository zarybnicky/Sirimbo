import { useForm } from "react-hook-form";
import { SelectElement } from 'components/SelectElement';
import { DataGrid } from '@mui/x-data-grid';
import { useFileListQuery } from "lib/graphql/Documents";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { formatFullDate } from "lib/format-date";
import { Item } from "components/layout/Item";

const categories = [
  { id: "1", label: "Schůze,\u{00A0}rady" },
  { id: "2", label: 'Soutěže' },
  { id: "3", label: 'Soustředění' },
  { id: "0", label: 'Ostatní' },
];

export default function FileListPage() {
  const { control, watch } = useForm<{ category: string; }>();
  const category = watch('category');

  const { data } = useFileListQuery({
    category: category ? parseInt(category, 10) : undefined,
  });

  return <Item>
    <Item.Titlebar title="Dokumenty">
      <SelectElement control={control} name="category" label="Kategorie" required options={categories} />
    </Item.Titlebar>

    <DataGrid
      pageSize={20}
      autoHeight={true}
      rows={data?.dokumenties?.nodes || []}
      columns={[
        {
          field: 'dName', headerName: 'Soubor', flex: 1,
          renderCell: ({ row }) => (
            <a target="_blank" rel="noreferrer" href={`/old/member/download?id=${row.id}`}>{row.dName}</a>
          ),
        },
        {
          field: 'category', headerName: 'Kategorie', width: 150,
          renderCell: ({ row }) => categories.find(x => x.id === row.dKategorie.toString())?.label,
        },
        {
          field: 'date', headerName: 'Přidáno', flex: 1,
          renderCell: ({ row }) => row.dTimestamp ? formatFullDate(new Date(row.dTimestamp)) : '',
        }
      ]}
    />
  </Item >;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
