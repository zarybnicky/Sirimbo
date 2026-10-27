'use client';

import { useAuth } from '@/lib/auth';
import { tenantCatalog } from '@/tenant/catalog';
import OlympLogo from '@/tenant/olymp/logo.webp';
import KometaLogo from '@/tenant/kometa/logo-white.webp';
import StarletLogo from '@/tenant/starlet/starlet-logo-mobile.png';
import Image from '@/ui/Image';
import { FormError } from '@/ui/form';
import { useSearchParams } from 'next/navigation';
import { useState } from 'react';

const clubs = [
  { id: 1, logo: OlympLogo },
  { id: 2, logo: KometaLogo },
  { id: 3, logo: StarletLogo },
];

export function EventLoginPrompt({ eventId }: { eventId: string }) {
  const auth = useAuth();
  const search = useSearchParams();
  const [dismissed, setDismissed] = useState(false);
  if (auth.isLoggedIn || dismissed) return null;

  return (
    <div className="grid gap-3">
      <div className="font-medium">Máte už účet? Přihlaste se přes svůj klub.</div>
      {search?.get('login') === 'failed' && (
        <FormError error="Přihlášení vypršelo nebo se nezdařilo. Vyberte klub a zkuste to znovu." />
      )}
      <form method="get" className="grid grid-cols-3 gap-2">
        <input type="hidden" name="event" value={eventId} />
        {clubs.map(({ id, logo }) => (
          <button
            key={id}
            type="submit"
            data-tenant={id}
            formAction={`${tenantCatalog[id]!.config.origin}/api/auth/handoff`}
            className={`min-w-0 cursor-pointer rounded-md hover:opacity-80 focus-visible:outline-2 focus-visible:outline-offset-4 focus-visible:outline-accent-9 ${id === 2 ? 'bg-[#BE9F69]' : 'bg-accent-9'}`}
          >
            <Image
              src={logo}
              alt={tenantCatalog[id]!.name}
              sizes="160px"
              className="h-16 w-full object-contain"
            />
          </button>
        ))}
      </form>
      <button
        type="button"
        className="justify-self-start text-sm text-neutral-11 underline underline-offset-2 hover:text-neutral-12"
        onClick={() => setDismissed(true)}
      >
        Nemám účet
      </button>
    </div>
  );
}
