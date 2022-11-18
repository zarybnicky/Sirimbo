import { useForm } from "react-hook-form";
import { SelectElement } from 'components/SelectElement';
import { DataGrid } from '@mui/x-data-grid';
import format from "date-fns/format";
import { useFileListQuery } from "lib/graphql/Documents";
import { Layout } from 'components/layout/Layout';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';

const categories = [
  { id: "1", label: "Schůze,\u{00A0}rady" },
  { id: "2", label: 'Soutěže' },
  { id: "3", label: 'Soustředění' },
  { id: "0", label: 'Ostatní' },
];

export default function FileListPage() {
  const { control, watch } = useForm<{ category: string; }>();
  const { data } = useFileListQuery({ category: parseInt(watch('category'), 10) });

  return <div className="container mx-auto max-w-5xl pt-12 pb-4">
    <div style={{ margin: '0 1rem 1rem auto', maxWidth: '12rem' }}>
      <SelectElement control={control} name="category" label="Kategorie" required options={categories} />
    </div>

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
          renderCell: ({ row }) => row.dTimestamp ? format(new Date(row.dTimestamp), 'd. M. y') : '',
        }
      ]}
    />
  </div>;
}

FileListPage.getLayout = (page: React.ReactElement) => <Layout>{page}</Layout>;

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
