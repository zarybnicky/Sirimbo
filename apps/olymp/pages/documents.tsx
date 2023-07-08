import { useForm } from 'react-hook-form';
import { ComboboxElement } from '@app/ui/Combobox';
import { FileListDocument } from '@app/graphql/Documents';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import { fullDateFormatter } from '@app/ui/format-date';
import { Card } from '@app/ui/Card';
import type { NextPageWithLayout } from 'pages/_app';
import { useQuery } from 'urql';
import { TitleBar } from '@app/ui/TitleBar';
import { origin } from '@app/graphql/query';

const categories = [
  { id: '1', label: 'Schůze,\u{00A0}rady' },
  { id: '2', label: 'Soutěže' },
  { id: '3', label: 'Soustředění' },
  { id: '0', label: 'Ostatní' },
];

const Page: NextPageWithLayout = () => {
  const { control, watch } = useForm<{ category: string }>();
  const category = watch('category');

  const [{ data }] = useQuery({
    query: FileListDocument,
    variables: {
      category: category ? parseInt(category, 10) : undefined,
    },
  });

  return (
    <div className="container p-4 lg:py-8">
      <TitleBar title="Dokumenty">
        <ComboboxElement
          align="end"
          control={control}
          name="category"
          placeholder="všechny dokumenty"
          options={categories}
        />
      </TitleBar>

      {data?.dokumenties?.nodes?.map((row, i) => (
        <Card key={i}>
          <a
            target="_blank"
            rel="noreferrer"
            href={`${origin}/member/download?id=${row.id}`}
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
    </div>
  );
};

Page.staticTitle = 'Dokumenty';
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
