import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { fromSlugArray } from 'lib/slugify';
import type { NextPageWithLayout } from 'pages/_app';
import { CoupleDocument, DeleteCoupleDocument } from 'lib/graphql/Couple';
import { LessonButton } from 'components/LessonButton';
import { CoupleList } from 'lib/entity-lists';
import { formatCoupleName } from 'lib/format-name';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const [{ data }] = useQuery({query: CoupleDocument, variables: { id },pause: !id });
  const item = data?.pary;
  if (!item) {
    return null;
  }
  return (
    <Item>
      <Item.Titlebar backHref="/admin/pary" title={formatCoupleName(item)}>
        <DeleteButton
          doc={DeleteCoupleDocument}
          id={id}
          onDelete={() => router.push('/admin/nastenka')}
          title="smazat pár"
        />
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
