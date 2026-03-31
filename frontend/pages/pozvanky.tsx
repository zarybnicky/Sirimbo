import { Layout } from '@/ui/Layout';
import {
  CreateInvitationDocument,
  PeopleWithAnotherAccountDocument,
  PeopleWithInvitationDocument,
  PeopleWithoutInvitationDocument,
} from '@/graphql/Invitation';
import { fullDateFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { PageHeader } from '@/ui/TitleBar';
import Link from 'next/link';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';

export default function InvitationOverviewPage() {
  const [{ data: withAnotherAccount }] = useQuery({
    query: PeopleWithAnotherAccountDocument,
  });
  const [{ data: withoutInvitation }] = useQuery({
    query: PeopleWithoutInvitationDocument,
  });
  const [{ data: withInvitation }] = useQuery({ query: PeopleWithInvitationDocument });
  const [, sendInvitation] = useMutation(CreateInvitationDocument);

  const bulkSendInvitations = useAsyncCallback(async () => {
    const sent = new Set<string>();
    for (const person of withoutInvitation?.peopleWithoutAccessOrInvitationList || []) {
      if (!person.email) continue;
      if (sent.has(person.email)) continue;
      sent.add(person.email);
      await sendInvitation({
        input: {
          personInvitation: {
            personId: person.id,
            email: person.email,
          },
        },
      });
    }
  });

  return (
    <Layout requireAdmin>
      <PageHeader title="Přehled pozvánek" />

      <div className="prose prose-accent">
        {(withAnotherAccount?.peopleWithoutAccessWithExistingAccountList || []).length >
          0 && (
          <>
            <h2>
              Osoby bez přístupu do systému - s jiným existujícím účtem podle e-mailu,
              stačí přiřadit
            </h2>
            <ul>
              {withAnotherAccount?.peopleWithoutAccessWithExistingAccountList?.map(
                (x) => (
                  <li key={x.id}>
                    <Link href={{ pathname: '/clenove/[id]', query: { id: x.id } }}>
                      {x.name}
                    </Link>
                  </li>
                ),
              )}
            </ul>
          </>
        )}

        {(
          withoutInvitation?.peopleWithoutAccessOrInvitationList?.filter(
            (x) => !x.email,
          ) || []
        ).length > 0 && (
          <>
            <h2>Osoby bez přístupu do systému - ještě nepozvaní, bez e-mailu</h2>
            <ul>
              {withoutInvitation?.peopleWithoutAccessOrInvitationList
                ?.filter((x) => !x.email)
                .map((x) => (
                  <li key={x.id}>
                    <Link href={{ pathname: '/clenove/[id]', query: { id: x.id } }}>
                      {x.name}
                    </Link>
                    {', vytvořen '}
                    {x.createdAt ? fullDateFormatter.format(new Date(x.createdAt)) : ''}
                    {x.email ? '' : <b>, chybí e-mail, není kam poslat pozvánku</b>}
                  </li>
                ))}
            </ul>
          </>
        )}

        {(
          withoutInvitation?.peopleWithoutAccessOrInvitationList?.filter(
            (x) => x.email,
          ) || []
        ).length > 0 && (
          <>
            <h2>Osoby bez přístupu do systému - ještě nepozvaní</h2>

            <SubmitButton
              type="button"
              onClick={bulkSendInvitations.execute}
              loading={bulkSendInvitations.loading}
            >
              Pozvat všechny
            </SubmitButton>

            <ul>
              {withoutInvitation?.peopleWithoutAccessOrInvitationList
                ?.filter((x) => x.email)
                .map((x) => (
                  <li key={x.id}>
                    <Link href={{ pathname: '/clenove/[id]', query: { id: x.id } }}>
                      {x.name}
                    </Link>
                    {', vytvořen '}
                    {x.createdAt ? fullDateFormatter.format(new Date(x.createdAt)) : ''}
                  </li>
                ))}
            </ul>
          </>
        )}

        {(withInvitation?.peopleWithoutAccessWithInvitationList || []).length > 0 && (
          <>
            <h2>Osoby bez přístupu do systému - již pozvaní</h2>
            <ul>
              {withInvitation?.peopleWithoutAccessWithInvitationList
                ?.toSorted((x, y) => x.createdAt.localeCompare(y.createdAt))
                .map((x) => (
                  <li key={x.id}>
                    <Link href={{ pathname: '/clenove/[id]', query: { id: x.id } }}>
                      {x.name}
                    </Link>
                    {', vytvořen '}
                    {x.createdAt ? fullDateFormatter.format(new Date(x.createdAt)) : ''}
                    {', pozvánka odeslána '}
                    {x.personInvitationsList
                      .map((x) => fullDateFormatter.format(new Date(x.createdAt)))
                      .join(', ')}
                  </li>
                ))}
            </ul>
          </>
        )}

        {(withAnotherAccount?.peopleWithoutAccessWithExistingAccountList || []).length ===
          0 &&
          (withoutInvitation?.peopleWithoutAccessOrInvitationList || []).length === 0 &&
          (withInvitation?.peopleWithoutAccessWithInvitationList || []).length === 0 && (
            <div>✅ Všechny v pořádku</div>
          )}
      </div>
    </Layout>
  );
}
