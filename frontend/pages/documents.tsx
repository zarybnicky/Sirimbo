import { useForm } from 'react-hook-form';
import { SelectElement } from 'components/SelectElement';
import { useFileListQuery } from 'lib/graphql/Documents';
import {PermissionKey, PermissionLevel} from 'lib/data/use-permissions';
import { fullDateFormatter } from 'lib/format-date';
import { Item } from 'components/layout/Item';
import { Card } from 'components/Card';
import { type NextPageWithLayout } from 'pages/_app';

const categories = [
  { id: '1', label: 'Schůze,\u{00A0}rady' },
  { id: '2', label: 'Soutěže' },
  { id: '3', label: 'Soustředění' },
  { id: '0', label: 'Ostatní' },
];

const Page: NextPageWithLayout = () => {
  const { control, watch } = useForm<{ category: string }>();
  const category = watch('category');

  const { data } = useFileListQuery({
    category: category ? parseInt(category, 10) : undefined,
  });

  return (
    <Item className="col-full-width bg-stone-100">
      <Item.Titlebar title="Dokumenty">
        <SelectElement
          control={control}
          name="category"
          label="Kategorie"
          required
          options={categories}
        />
      </Item.Titlebar>

      {data?.dokumenties?.nodes?.map((row, i) => (
        <Card key={i}>
          <a
            target="_blank"
            rel="noreferrer"
            href={`/member/download?id=${row.id}`}
            className="flex justify-between"
          >
            <span>{row.dName}</span>
            <div className="flex gap-4">
              <span>
                {row.dTimestamp ? fullDateFormatter.format(new Date(row.dTimestamp)) : ''}
              </span>
              <span>
                {categories.find((x) => x.id === row.dKategorie.toString())?.label}
              </span>
            </div>
          </a>
        </Card>
      ))}
    </Item>
  );
}

Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
