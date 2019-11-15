<?php
class User
{
    public static function fromArray(array $x): User
    {
        $user = new self();
        $user->setId($x['u_id']);
        $user->setLogin($x['u_login']);
        $user->setPassword($x['u_pass']);
        $user->setName($x['u_jmeno']);
        $user->setSurname($x['u_prijmeni']);
        $user->setGender($x['u_pohlavi']);
        $user->setEmail($x['u_email']);
        $user->setPhone($x['u_telefon']);
        $user->setBirthDate($x['u_narozeni']);
        $user->setNotes($x['u_poznamky']);
        $user->setUpdatedAt($x['u_timestamp']);
        $user->setPermissionGroup($x['u_group']);
        $user->setTrainingGroup($x['u_skupina']);
        $user->setDancer($x['u_dancer']);
        $user->setBanned($x['u_ban']);
        $user->setLocked($x['u_lock']);
        $user->setConfirmed($x['u_confirmed']);
        $user->setSystem($x['u_system']);
        $user->setStreet($x['u_street']);
        $user->setConscriptionNumber($x['u_conscription_number']);
        $user->setOrientationNumber($x['u_orientation_number']);
        $user->setDistrict($x['u_district']);
        $user->setCity($x['u_city']);
        $user->setPostalCode($x['u_postal_code']);
        $user->setNationality($x['u_nationality']);
        $user->setMemberSince($x['u_member_since']);
        $user->setMemberUntil($x['u_member_until']);
        $user->setCreatedAt($x['u_created_at']);
        $user->setTeacher($x['u_teacher']);
        $user->setGdprSignedAt($x['u_gdpr_signed_at']);
        return $user;
    }

    public function toArray(): array
    {
        return [
            'u_id' => $this->getId(),
            'u_login' => $this->getLogin(),
            'u_pass' => $this->getPassword(),
            'u_jmeno' => $this->getName(),
            'u_prijmeni' => $this->getSurname(),
            'u_pohlavi' => $this->getGender(),
            'u_email' => $this->getEmail(),
            'u_telefon' => $this->getPhone(),
            'u_narozeni' => $this->getBirthDate(),
            'u_poznamky' => $this->getNotes(),
            'u_timestamp' => $this->getCreatedAt(),
            'u_group' => $this->getPermissionGroup(),
            'u_skupina' => $this->getTrainingGroup(),
            'u_dancer' => $this->getDancer(),
            'u_ban' => $this->getBanned(),
            'u_lock' => $this->getLocked(),
            'u_confirmed' => $this->getConfirmed(),
            'u_system' => $this->getSystem(),
            'u_street' => $this->getStreet(),
            'u_conscription_number' => $this->getConscriptionNumber(),
            'u_orientation_number' => $this->getOrientationNumber(),
            'u_district' => $this->getDistrict(),
            'u_city' => $this->getCity(),
            'u_postal_code' => $this->getPostalCode(),
            'u_nationality' => $this->getNationality(),
            'u_member_since' => $this->getMemberSince(),
            'u_member_until' => $this->getMemberUntil(),
            'u_created_at' => $this->getCreatedAt(),
            'u_teacher' => $this->getTeacher(),
            'u_gdpr_signed_at' => $this->getGdprSignedAt(),
        ];
    }

    public function isValid()
    {
        return preg_match(
            "/^[A-Z0-9._%-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$/i",
            $this->getEmail()
        ) && preg_match(
            "/^((\+|00)\d{3})?( ?\d{3}){3}$/",
            $this->getPhone()
        ) && preg_match(
            "/^((?:19|20)\d\d)-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$/",
            $this->getBirthDate()
        );
    }

    /**
     * @var int
     */
    protected $id;

    public function getId(): int
    {
        return $this->id;
    }

    public function setId(int $id)
    {
        $this->id = $id;
    }

    /**
     * @var string
     */
    protected $login;

    public function getLogin(): string
    {
        return $this->login;
    }

    public function setLogin(string $login)
    {
        $this->login = $login;
    }

    /**
     * @var string
     */
    protected $password;

    public function getPassword(): string
    {
        return $this->password;
    }

    public function setPassword(string $password)
    {
        $this->password = $password;
    }

    /**
     * @var string
     */
    protected $name;

    public function getName(): string
    {
        return $this->name;
    }

    public function setName(string $name)
    {
        $this->name = $name;
    }

    /**
     * @var string
     */
    protected $surname;

    public function getSurname(): string
    {
        return $this->surname;
    }

    public function setSurname(string $surname)
    {
        $this->surname = $surname;
    }

    public function getFullName()
    {
        return $this->getSurname() . ', ' . $this->getName();
    }

    /**
     * @var string
     */
    protected $gender;

    public function getGender(): string
    {
        return $this->gender;
    }

    public function setGender(string $gender)
    {
        $this->gender = $gender;
    }

    /**
     * @var string
     */
    protected $email;

    public function getEmail(): string
    {
        return $this->email;
    }

    public function setEmail(string $email)
    {
        $this->email = $email;
    }

    /**
     * @var string
     */
    protected $phone;

    public function getPhone(): string
    {
        return $this->phone;
    }

    public function setPhone(string $phone)
    {
        $this->phone = $phone;
    }

    /**
     * @var string
     */
    protected $birthDate;

    public function getBirthDate(): string
    {
        return $this->birthDate;
    }

    public function setBirthDate(string $birthDate)
    {
        $this->birthDate = $birthDate;
    }

    public function getBirthYear(): string
    {
        return explode('-', $this->getBirthDate())[0];
    }

    /**
     * @var string
     */
    protected $notes;

    public function getNotes(): string
    {
        return $this->notes;
    }

    public function setNotes(string $notes)
    {
        $this->notes = $notes;
    }

    /**
     * @var string
     */
    protected $updatedAt;

    public function getUpdatedAt(): string
    {
        return $this->updatedAt;
    }

    public function setUpdatedAt(string $updatedAt)
    {
        $this->updatedAt = $updatedAt;
    }

    /**
     * @var int
     */
    protected $permissionGroup = 0;

    public function getPermissionGroup(): int
    {
        return $this->permissionGroup;
    }

    public function setPermissionGroup(int $permissionGroup)
    {
        $this->permissionGroup = $permissionGroup;
    }

    /**
     * @var int
     */
    protected $trainingGroup;

    public function getTrainingGroup(): int
    {
        return $this->trainingGroup;
    }

    public function setTrainingGroup(int $trainingGroup)
    {
        $this->trainingGroup = $trainingGroup;
    }

    /**
     * @var bool
     */
    protected $dancer;

    public function getDancer(): bool
    {
        return $this->dancer;
    }

    public function setDancer(bool $dancer)
    {
        $this->dancer = $dancer;
    }

    /**
     * @var bool
     */
    protected $banned;

    public function getBanned(): bool
    {
        return $this->banned;
    }

    public function setBanned(bool $banned)
    {
        $this->banned = $banned;
    }

    /**
     * @var bool
     */
    protected $locked;

    public function getLocked(): bool
    {
        return $this->locked;
    }

    public function setLocked(bool $locked)
    {
        $this->locked = $locked;
    }

    /**
     * @var bool
     */
    protected $confirmed;

    public function getConfirmed(): bool
    {
        return $this->confirmed;
    }

    public function setConfirmed(bool $confirmed)
    {
        $this->confirmed = $confirmed;
    }

    /**
     * @var bool
     */
    protected $system;

    public function getSystem(): bool
    {
        return $this->system;
    }

    public function setSystem(bool $system)
    {
        $this->system = $system;
    }

    /**
     * @var string
     */
    protected $street;

    public function getStreet(): string
    {
        return $this->street;
    }

    public function setStreet(string $street)
    {
        $this->street = $street;
    }

    /**
     * @var string
     */
    protected $conscriptionNumber;

    public function getConscriptionNumber(): string
    {
        return $this->conscriptionNumber;
    }

    public function setConscriptionNumber(string $conscriptionNumber)
    {
        $this->conscriptionNumber = $conscriptionNumber;
    }

    /**
     * @var string
     */
    protected $orientationNumber;

    public function getOrientationNumber(): string
    {
        return $this->orientationNumber;
    }

    public function setOrientationNumber(string $orientationNumber)
    {
        $this->orientationNumber = $orientationNumber;
    }

    /**
     * @var string
     */
    protected $district;

    public function getDistrict(): string
    {
        return $this->district;
    }

    public function setDistrict(string $district)
    {
        $this->district = $district;
    }

    /**
     * @var string
     */
    protected $city;

    public function getCity(): string
    {
        return $this->city;
    }

    public function setCity(string $city)
    {
        $this->city = $city;
    }

    /**
     * @var string
     */
    protected $postalCode;

    public function getPostalCode(): string
    {
        return $this->postalCode;
    }

    public function setPostalCode(string $postalCode)
    {
        $this->postalCode = $postalCode;
    }

    /**
     * @var string
     */
    protected $nationality;

    public function getNationality(): string
    {
        return $this->nationality;
    }

    public function setNationality(string $nationality)
    {
        $this->nationality = $nationality;
    }

    /**
     * @var string|null
     */
    protected $memberSince;

    public function getMemberSince(): ?string
    {
        return $this->memberSince;
    }

    public function setMemberSince(?string $memberSince)
    {
        $this->memberSince = $memberSince;
    }

    /**
     * @var string|null
     */
    protected $memberUntil;

    public function getMemberUntil(): ?string
    {
        return $this->memberUntil;
    }

    public function setMemberUntil(?string $memberUntil)
    {
        $this->memberUntil = $memberUntil;
    }

    /**
     * @var string
     */
    protected $createdAt;

    public function getCreatedAt(): string
    {
        return $this->createdAt;
    }

    public function setCreatedAt(string $createdAt)
    {
        $this->createdAt = $createdAt;
    }

    /**
     * @var bool
     */
    protected $teacher;

    public function getTeacher(): bool
    {
        return $this->teacher;
    }

    public function setTeacher(bool $teacher)
    {
        $this->teacher = $teacher;
    }

    /**
     * @var string|null
     */
    protected $gdprSignedAt;

    public function getGdprSignedAt(): ?string
    {
        return $this->gdprSignedAt;
    }

    public function setGdprSignedAt(?string $gdprSignedAt)
    {
        $this->gdprSignedAt = $gdprSignedAt;
    }

    public static function crypt($passwd)
    {
        $fix = md5('######TK.-.OLYMP######');
        return sha1($fix . $passwd . $fix);
    }

    public static function varSymbol($id)
    {
        return str_pad($id, 6, '0', STR_PAD_LEFT);
    }
}
