import { useForm } from "react-hook-form";
import { useFileListQuery } from 'lib/graphql';
import { SelectElement } from "react-hook-form-mui";
import { Container } from "@mui/material";
import { DataGrid } from '@mui/x-data-grid';
import format from "date-fns/format";
import { useRequireUserLoggedIn } from "lib/route-guards";

const categories = [
  { id: 1, label: "Schůze,\u{00A0}rady" },
  { id: 2, label: 'Soutěže' },
  { id: 3, label: 'Soustředění' },
  { id: 0, label: 'Ostatní' },
];

export default function FileListPage() {
  useRequireUserLoggedIn();
  const { control, watch } = useForm<{ category: number; }>();
  const { data } = useFileListQuery({ category: watch('category') });

  return <Container maxWidth="lg" className="pt-8">
    <div style={{ margin: '0 1rem 1rem auto', maxWidth: '12rem' }}>
      <SelectElement control={control} name="category" label="Kategorie" required options={categories} />
    </div>

    <DataGrid
      pageSize={20}
      autoHeight={true}
      getRowId={row => row.dId}
      rows={data?.dokumenties?.nodes || []}
      columns={[
        {
          field: 'dName', headerName: 'Soubor', flex: 1,
          renderCell: ({ row }) => (
            <a target="_blank" rel="noreferrer" href={`/old/member/download?id=${row.dId}`}>{row.dName}</a>
          ),
        },
        {
          field: 'category', headerName: 'Kategorie', width: 150,
          renderCell: ({ row }) => categories.find(x => x.id === row.dKategorie)?.label,
        },
        {
          field: 'date', headerName: 'Přidáno', flex: 1,
          renderCell: ({ row }) => row.dTimestamp ? format(new Date(row.dTimestamp), 'd. M. y') : '',
        }
      ]}
    />
  </Container>;
}
