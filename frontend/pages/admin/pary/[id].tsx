import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';
import { useGqlMutation, useGqlQuery } from 'lib/query';
import { CoupleDocument, DeleteCoupleDocument } from 'lib/graphql/Couple';
import { LessonButton } from 'components/LessonButton';
import { CoupleList } from 'lib/entity-lists';
import { formatCoupleName } from 'lib/format-name';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
    const { data } = useGqlQuery(CoupleDocument, { id }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useGqlMutation(DeleteCoupleDocument, {
    onSuccess: () => router.push('/admin/nastenka'),
  });
  const item = data?.pary;
  if (!item) {
    return null;
  }
  return (
    <Item>
      <Item.Titlebar backHref="/admin/pary" title={formatCoupleName(item)}>
        <DeleteButton onDelete={() => doDelete({ id })} title="smazat pár" />
      </Item.Titlebar>

      Minulé lekce
      {data?.pary?.rozpisItemsByRiPartner?.nodes.map((item) => (
        <LessonButton key={item.id} schedule={item.rozpiByRiIdRodic!} lesson={item} showTrainer showDate />
      ))}
    </Item>
  );
};

Page.list = <CoupleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];
Page.staticTitle = "Páry";

export default Page;
