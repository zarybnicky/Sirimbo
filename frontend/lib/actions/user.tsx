import { Link2 } from 'lucide-react';
import { defineActions } from '@/lib/actions';
import { LinkUserToPersonForm } from '@/ui/forms/LinkUserToPersonForm';

export const userActions = defineActions<{ id: string }>()([
  {
    id: 'user.linkPerson',
    group: 'primary',
    label: 'Přiřadit osobu',
    icon: Link2,
    requireAdmin: true,
    render: ({ item }) => <LinkUserToPersonForm userId={item.id} />,
  },
]);
