import { Pencil } from 'lucide-react';
import { defineActions } from '@/lib/actions';
import { LocationForm } from '@/ui/forms/LocationForm';

export const locationActions = defineActions<{ id: string }>()([
  {
    id: 'location.edit',
    label: 'Upravit',
    icon: Pencil,
    group: 'primary',
    requireAdmin: true,
    render: ({ item }) => <LocationForm id={item.id} />,
  },
]);
