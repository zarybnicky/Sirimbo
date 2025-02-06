import * as React from 'react';
import { CurrentUserDocument } from '@/graphql/CurrentUser';
import { useQuery } from 'urql';
import { tokenAtom } from '@/ui/state/auth';
import { useAtom } from 'jotai';
import { toast } from 'react-toastify';
import { Dialog, DialogContent } from '@/ui/dialog';
import { EditPersonForm } from '@/ui/forms/EditPersonForm';
import { buttonCls } from './style';
import { Edit } from 'lucide-react';
import type { PersonFragment } from '@/graphql/Person';

export function FillYourProfileReminder() {
  const [token] = useAtom(tokenAtom);
  const [person, setPerson] = React.useState<PersonFragment | null>(null);

  const [{ data: currentUser }] = useQuery({
    query: CurrentUserDocument,
    pause: !token,
  });

  React.useEffect(() => {
    const now = new Date().getTime() - 24 * 60 * 60 * 1000;
    const shouldCheck = now - 24 * 60 * 60 * 1000;
    for (const { person } of currentUser?.getCurrentUser?.userProxiesList ?? []) {
      if (!person) continue;

      const lastChecked = localStorage.getItem(`profile-checked-${person.id}`);
      if (lastChecked && lastChecked >= shouldCheck.toString()) continue;

      localStorage.setItem(`profile-checked-${person.id}`, now.toString());
      if (
        person.email &&
        person.phone &&
        person.nationality &&
        person.birthDate &&
        person.taxIdentificationNumber
        /* person.address?.street &&
* person.address?.city &&
* person.address?.postalCode */
      )
        continue;

      toast.warn(
        <>
          Vyplňte prosím chybějící údaje u osoby {person.name}:
          <button
            type="button"
            className={buttonCls({ variant: 'outline' })}
            onClick={() => setPerson(person)}
          >
            <Edit />
            Upravit osobu
          </button>
        </>,
      );
    }
  }, [currentUser]);

  return (
    <Dialog open={!!person} onOpenChange={() => setPerson(null)}>
      {person && (
        <DialogContent
          className="sm:max-w-2xl"
          onPointerDownOutside={(e) => e.preventDefault()}
        >
          <EditPersonForm data={person} />
        </DialogContent>
      )}
    </Dialog>
  );
}
