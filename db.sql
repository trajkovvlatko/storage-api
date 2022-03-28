--
-- PostgreSQL database dump
--

-- Dumped from database version 12.9 (Ubuntu 12.9-0ubuntu0.20.04.1)
-- Dumped by pg_dump version 12.9 (Ubuntu 12.9-0ubuntu0.20.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: colors; Type: TABLE; Schema: public; Owner: vlatko
--

CREATE TABLE public.colors (
    id integer NOT NULL,
    label character varying(255) NOT NULL
);


ALTER TABLE public.colors OWNER TO vlatko;

--
-- Name: colors_id_seq; Type: SEQUENCE; Schema: public; Owner: vlatko
--

CREATE SEQUENCE public.colors_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.colors_id_seq OWNER TO vlatko;

--
-- Name: colors_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: vlatko
--

ALTER SEQUENCE public.colors_id_seq OWNED BY public.colors.id;


--
-- Name: drawers; Type: TABLE; Schema: public; Owner: vlatko
--

CREATE TABLE public.drawers (
    id integer NOT NULL,
    level integer NOT NULL,
    note character varying(255) NOT NULL,
    storage_unit_id integer NOT NULL,
    user_id integer NOT NULL
);


ALTER TABLE public.drawers OWNER TO vlatko;

--
-- Name: drawers_id_seq; Type: SEQUENCE; Schema: public; Owner: vlatko
--

CREATE SEQUENCE public.drawers_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.drawers_id_seq OWNER TO vlatko;

--
-- Name: drawers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: vlatko
--

ALTER SEQUENCE public.drawers_id_seq OWNED BY public.drawers.id;


--
-- Name: item_types; Type: TABLE; Schema: public; Owner: vlatko
--

CREATE TABLE public.item_types (
    id integer NOT NULL,
    label character varying(255) NOT NULL
);


ALTER TABLE public.item_types OWNER TO vlatko;

--
-- Name: item_types_id_seq; Type: SEQUENCE; Schema: public; Owner: vlatko
--

CREATE SEQUENCE public.item_types_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.item_types_id_seq OWNER TO vlatko;

--
-- Name: item_types_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: vlatko
--

ALTER SEQUENCE public.item_types_id_seq OWNED BY public.item_types.id;


--
-- Name: items; Type: TABLE; Schema: public; Owner: vlatko
--

CREATE TABLE public.items (
    id integer NOT NULL,
    user_id integer NOT NULL,
    drawer_id integer NOT NULL,
    color_id integer NOT NULL,
    item_type_id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE public.items OWNER TO vlatko;

--
-- Name: items_id_seq; Type: SEQUENCE; Schema: public; Owner: vlatko
--

CREATE SEQUENCE public.items_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.items_id_seq OWNER TO vlatko;

--
-- Name: items_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: vlatko
--

ALTER SEQUENCE public.items_id_seq OWNED BY public.items.id;


--
-- Name: rooms; Type: TABLE; Schema: public; Owner: vlatko
--

CREATE TABLE public.rooms (
    id integer NOT NULL,
    name character varying DEFAULT '255'::character varying NOT NULL,
    user_id integer NOT NULL
);


ALTER TABLE public.rooms OWNER TO vlatko;

--
-- Name: rooms_id_seq; Type: SEQUENCE; Schema: public; Owner: vlatko
--

CREATE SEQUENCE public.rooms_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.rooms_id_seq OWNER TO vlatko;

--
-- Name: rooms_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: vlatko
--

ALTER SEQUENCE public.rooms_id_seq OWNED BY public.rooms.id;


--
-- Name: storage_units; Type: TABLE; Schema: public; Owner: vlatko
--

CREATE TABLE public.storage_units (
    id integer NOT NULL,
    user_id integer NOT NULL,
    room_id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE public.storage_units OWNER TO vlatko;

--
-- Name: storages_id_seq; Type: SEQUENCE; Schema: public; Owner: vlatko
--

CREATE SEQUENCE public.storages_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.storages_id_seq OWNER TO vlatko;

--
-- Name: storages_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: vlatko
--

ALTER SEQUENCE public.storages_id_seq OWNED BY public.storage_units.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: vlatko
--

CREATE TABLE public.users (
    id integer NOT NULL,
    email character varying(255) NOT NULL,
    password character varying(255) NOT NULL
);


ALTER TABLE public.users OWNER TO vlatko;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: vlatko
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO vlatko;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: vlatko
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: colors id; Type: DEFAULT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.colors ALTER COLUMN id SET DEFAULT nextval('public.colors_id_seq'::regclass);


--
-- Name: drawers id; Type: DEFAULT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.drawers ALTER COLUMN id SET DEFAULT nextval('public.drawers_id_seq'::regclass);


--
-- Name: item_types id; Type: DEFAULT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.item_types ALTER COLUMN id SET DEFAULT nextval('public.item_types_id_seq'::regclass);


--
-- Name: items id; Type: DEFAULT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.items ALTER COLUMN id SET DEFAULT nextval('public.items_id_seq'::regclass);


--
-- Name: rooms id; Type: DEFAULT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.rooms ALTER COLUMN id SET DEFAULT nextval('public.rooms_id_seq'::regclass);


--
-- Name: storage_units id; Type: DEFAULT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.storage_units ALTER COLUMN id SET DEFAULT nextval('public.storages_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: colors colors_pkey; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.colors
    ADD CONSTRAINT colors_pkey PRIMARY KEY (id);


--
-- Name: drawers drawers_pkey; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.drawers
    ADD CONSTRAINT drawers_pkey PRIMARY KEY (id);


--
-- Name: item_types item_types_pkey; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.item_types
    ADD CONSTRAINT item_types_pkey PRIMARY KEY (id);


--
-- Name: items items_pkey; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT items_pkey PRIMARY KEY (id);


--
-- Name: rooms rooms_pkey; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.rooms
    ADD CONSTRAINT rooms_pkey PRIMARY KEY (id);


--
-- Name: storage_units storages_pkey; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.storage_units
    ADD CONSTRAINT storages_pkey PRIMARY KEY (id);


--
-- Name: users users_email_unique; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_email_unique UNIQUE (email);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: colors_label_idx; Type: INDEX; Schema: public; Owner: vlatko
--

CREATE UNIQUE INDEX colors_label_idx ON public.colors USING btree (label);


--
-- Name: drawers_storage_id_idx; Type: INDEX; Schema: public; Owner: vlatko
--

CREATE INDEX drawers_storage_id_idx ON public.drawers USING btree (storage_unit_id);


--
-- Name: drawers_user_id_idx; Type: INDEX; Schema: public; Owner: vlatko
--

CREATE INDEX drawers_user_id_idx ON public.drawers USING btree (user_id);


--
-- Name: item_types_label_idx; Type: INDEX; Schema: public; Owner: vlatko
--

CREATE UNIQUE INDEX item_types_label_idx ON public.item_types USING btree (label);


--
-- Name: rooms_user_id_idx; Type: INDEX; Schema: public; Owner: vlatko
--

CREATE INDEX rooms_user_id_idx ON public.rooms USING btree (user_id);


--
-- Name: storages_room_id_idx; Type: INDEX; Schema: public; Owner: vlatko
--

CREATE INDEX storages_room_id_idx ON public.storage_units USING btree (room_id);


--
-- Name: storages_user_id_idx; Type: INDEX; Schema: public; Owner: vlatko
--

CREATE INDEX storages_user_id_idx ON public.storage_units USING btree (user_id);


--
-- Name: items color_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT color_id_fkey FOREIGN KEY (color_id) REFERENCES public.colors(id) ON DELETE CASCADE;


--
-- Name: items drawer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT drawer_id_fkey FOREIGN KEY (drawer_id) REFERENCES public.drawers(id) ON DELETE CASCADE;


--
-- Name: items item_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT item_type_id_fkey FOREIGN KEY (item_type_id) REFERENCES public.item_types(id) ON DELETE CASCADE;


--
-- Name: storage_units room_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.storage_units
    ADD CONSTRAINT room_id_fkey FOREIGN KEY (room_id) REFERENCES public.rooms(id) ON DELETE CASCADE;


--
-- Name: drawers storage_unit_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.drawers
    ADD CONSTRAINT storage_unit_id_fkey FOREIGN KEY (storage_unit_id) REFERENCES public.storage_units(id) ON DELETE CASCADE;


--
-- Name: drawers user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.drawers
    ADD CONSTRAINT user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: rooms user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.rooms
    ADD CONSTRAINT user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: storage_units user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.storage_units
    ADD CONSTRAINT user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- Name: items user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: vlatko
--

ALTER TABLE ONLY public.items
    ADD CONSTRAINT user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id) ON DELETE CASCADE;


--
-- PostgreSQL database dump complete
--

