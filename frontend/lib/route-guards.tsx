import React from "react";
import { useRouter } from "next/router";
import { useAuth } from "./data/use-auth";

export const useRequireUserLoggedIn = () => {
  const router = useRouter();
  const { user, isLoading } = useAuth();

  React.useEffect(() => {
    if (!user && !isLoading) {
      router.replace("/login");
    }
  }, [user, isLoading]);
};

export const useRequireUserLoggedOut = () => {
  const router = useRouter();
  const { user, isLoading } = useAuth();

  React.useEffect(() => {
    if (user && !isLoading) {
      router.replace("/dashboard");
    }
  }, [user, isLoading]);
};
