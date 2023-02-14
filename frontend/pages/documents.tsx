import { useForm } from "react-hook-form";
import { SelectElement } from 'components/SelectElement';
import { useFileListQuery } from "lib/graphql/Documents";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { formatFullDate } from "lib/format-date";
import { Item } from "components/layout/Item";
import { Card } from "components/Card";

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

    {(data?.dokumenties?.nodes || []).map((row, i) => <Card key={i}>
      <a target="_blank" rel="noreferrer" href={`/old/member/download?id=${row.id}`}>{row.dName}</a>
      {categories.find(x => x.id === row.dKategorie.toString())?.label}
      {row.dTimestamp ? formatFullDate(new Date(row.dTimestamp)) : ''}
    </Card>)}
  </Item>;
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
